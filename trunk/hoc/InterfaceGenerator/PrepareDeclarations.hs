module PrepareDeclarations(
        prepareDeclarations,
        PreparedDeclarations(..),
        ClassInfo(..),
        SelectorLocation(..),
        MangledSelector(..), msName,
        
    ) where

import SyntaxTree
import BindingScript
import CTypeToHaskell
import Headers(HeaderInfo(..), ModuleName)
import Enums

import HOC.NameCaseChange
import HOC.SelectorNameMangling(mangleSelectorName)

import Control.Monad(when)
import Data.Set(Set, mkSet, setToList, union, minusSet, unionManySets,
                emptySet, elementOf)
import Data.FiniteMap
import qualified Data.HashTable as HashTable
import Data.Maybe(maybeToList, fromMaybe, mapMaybe)
import Data.List(partition,isPrefixOf)
import Data.Char(isLower)

data PreparedDeclarations = PreparedDeclarations {
        pdCleanClassInfos :: [(String, ClassInfo)],
        pdCleanClassInfoHash :: HashTable.HashTable String ClassInfo, {- used read only -}
        pdAllInstanceSels :: [(ClassInfo, [(MangledSelector, SelectorLocation)])],
        pdAllClassSels :: [(ClassInfo, [(MangledSelector, SelectorLocation)])],
        pdEnumTypeDefinitions :: FiniteMap ModuleName [EnumType],
        pdExternVarDeclarations :: FiniteMap ModuleName [(HType, String, String)],
        pdExternFunDeclarations :: FiniteMap ModuleName [MangledSelector],
        pdTypeEnvironment :: TypeEnvironment
    }

data SelectorLocation = SelectorLocation {
                                slDefinitionLocation :: ModuleName,
                                slHasInstanceLocation :: ModuleName
                            }
    deriving(Show, Eq)

data ClassInfo = ClassInfo {
        ciProtocol :: Bool,
        ciName :: String,
        ciSuper :: Maybe String,
        ciProtocols :: Set String,
        ciNewProtocols :: Set String,
        ciDefinedIn :: ModuleName,
        ciInstanceMethods :: FiniteMap Selector SelectorLocation,
        ciClassMethods :: FiniteMap Selector SelectorLocation,
        ciNewInstanceMethods :: FiniteMap Selector SelectorLocation,
        ciNewClassMethods :: FiniteMap Selector SelectorLocation
    }
    deriving(Show)
    
classInfoForDeclaration (moduleName, SelectorList (Interface name super protocols) methods) =
    Just $ (nameToUppercase name, ClassInfo {
        ciProtocol = False,
        ciName = nameToUppercase name,
        ciSuper = fmap nameToUppercase super,
        ciProtocols = mkSet (map nameToUppercase protocols),
        ciDefinedIn = moduleName,
        ciInstanceMethods = listToFM [ (sel, SelectorLocation moduleName moduleName)
                                     | InstanceMethod sel <- methods ],
        ciClassMethods = listToFM [ (sel, SelectorLocation moduleName moduleName)
                                  | ClassMethod sel <- methods ],
        ciNewProtocols = error "ciNewProtocols 1",
        ciNewInstanceMethods = error "ciNewInstanceMethods 1",
        ciNewClassMethods = error "ciNewClassMethods 1"
    })
classInfoForDeclaration (moduleName, SelectorList (Protocol name protocols) methods) =
    Just $ (nameToUppercase name ++ "Protocol", ClassInfo {
        ciProtocol = True,
        ciName = nameToUppercase name ++ "Protocol",
        ciSuper = Nothing,
        ciProtocols = mkSet (map nameToUppercase protocols),
        ciDefinedIn = moduleName,
        ciInstanceMethods = listToFM [ (sel, SelectorLocation moduleName cantHappen)
                                     | InstanceMethod sel <- methods ],
        ciClassMethods = listToFM [ (sel, SelectorLocation moduleName cantHappen)
                                  | ClassMethod sel <- methods ],
        ciNewProtocols = error "ciNewProtocols 2",
        ciNewInstanceMethods = error "ciNewInstanceMethods 2",
        ciNewClassMethods = error "ciNewClassMethods 2"
    })
    where
        cantHappen = error "internal error: protocol asked for location of instance decl"

    
classInfoForDeclaration _ = Nothing

updateClassInfoForCategory
    infos
    (moduleName, SelectorList (Category className catName moreProtocols) methods) = do
        mbClassInfo <- HashTable.lookup infos className
        case mbClassInfo of
            Nothing -> putStrLn $ moduleName ++ ": category " ++ className ++
                   "(" ++ catName ++ ") - class undefined"
            Just classInfo -> do                
                let classInfo' = classInfo {
                        ciProtocols = ciProtocols classInfo `union` mkSet moreProtocols,
                        ciInstanceMethods =
                            addListToFM_C (\old new -> old)
                                            (ciInstanceMethods classInfo)
                                            [ (sel, SelectorLocation moduleName moduleName)
                                            | InstanceMethod sel <- methods ],
                        ciClassMethods =
                            addListToFM_C (\old new -> old)
                                            (ciClassMethods classInfo)
                                            [ (sel, SelectorLocation moduleName moduleName)
                                            | ClassMethod sel <- methods ]
                    }
                
                HashTable.delete infos className
                HashTable.insert infos className classInfo'

updateClassInfoForCategory _ _ = return ()

cleanClassInfo outInfos inInfos name =
    do
        doneInfo <- HashTable.lookup outInfos name
        case doneInfo of
            Just done -> return ()
            Nothing -> do
                mbCi <- HashTable.lookup inInfos name
                case mbCi of
                    Just ci -> do
                        (mbSuper, protocols, recheck) <- cleanSuper ci
                        if recheck
                            then cleanClassInfo outInfos inInfos name
                            else do
                                -- putStrLn name
                                let ci' = cleanClassInfo' ci mbSuper protocols
                                HashTable.insert outInfos name ci'
                    Nothing -> do
                        fail $ "Couldn't find class: " ++ name
                        
    where
        cleanSuper ci = do
            (mbSuper,superRecheck) <- case (ciSuper ci) of
                Nothing -> return (Nothing,False)   
                Just super -> do
                    (super', recheck) <- findOrClean super
                    return (Just super', recheck)
            (protocols,protoRecheck) <- fmap unzip $
                                        mapM findOrClean $
                                        map (++"Protocol") $
                                        setToList $
                                        ciProtocols ci
            return (mbSuper, protocols, or (superRecheck : protoRecheck))
        findOrClean name = do
            info <- HashTable.lookup outInfos name
            case info of
                Just info -> return (info, False)
                Nothing -> do
                    cleanClassInfo outInfos inInfos name
                    findOrClean name
          
cleanClassInfo' info mbSuperInfo protocolInfos
    | ciProtocol info =
        info {
            ciInstanceMethods = foldl1 plusFM $
                                map ciInstanceMethods $
                                info : protocolInfos,
            ciClassMethods = foldl1 plusFM $
                             map ciClassMethods $
                             info : protocolInfos,
            ciNewInstanceMethods =
                ciInstanceMethods info `minusFM`
                (unionProtocols ciInstanceMethods),
            ciNewClassMethods =
                ciClassMethods info `minusFM`
                (unionProtocols ciClassMethods),
            ciProtocols = ciProtocols info `union` protocolsAdoptedByAdoptedProtocols,
            ciNewProtocols = ciProtocols info `minusSet` protocolsAdoptedByAdoptedProtocols
        }
    | otherwise =
        info {
            ciInstanceMethods = foldl1 plusFM $
                                map ciInstanceMethods $
                                info : (maybeToList mbSuperInfo) ++ protocolInfos,
            ciClassMethods = foldl1 plusFM $
                             map ciClassMethods $
                             info : (maybeToList mbSuperInfo) ++ protocolInfos,
            ciNewInstanceMethods = (ciInstanceMethods info `plusFM_proto`
                                       (unionProtocols ciInstanceMethods))
                                   `minusFM` super ciInstanceMethods,
            ciNewClassMethods = (ciClassMethods info `plusFM_proto`
                                    (unionProtocols ciClassMethods))
                                `minusFM` super ciClassMethods,
            ciProtocols = ciProtocols info
                          `union` protocolsAdoptedByAdoptedProtocols
                          `union` protocolsAdoptedBySuper,
            ciNewProtocols = ciProtocols info   
                             `union` protocolsAdoptedByAdoptedProtocols
                             `minusSet` protocolsAdoptedBySuper
        }
        where
            super extract = case mbSuperInfo of
                Just superInfo -> extract superInfo
                Nothing -> emptyFM
            unionProtocols extract = foldl plusFM emptyFM $
                                     map extract protocolInfos
            plusFM_proto cls proto = plusFM_C (\(SelectorLocation _ inst)
                                                (SelectorLocation def _)
                                              -> SelectorLocation def {-inst-} (ciDefinedIn info))
                                      -- * All selectors that are part of a protocol
                                      -- should be declared where the protocol is declared.
                                      -- * The method instances should be where the class itself
                                      -- with the protocol adoption is, not in a category.
                                      -- Otherwise, the context for the protocol instance declaration
                                      -- won't be available when the protocol is adopted.
                                              cls
                                              (mapFM (\sel (SelectorLocation def _)
                                                     -> SelectorLocation def (ciDefinedIn info))
                                                     proto)
            protocolsAdoptedByAdoptedProtocols = unionManySets $
                                      map ciProtocols $
                                      protocolInfos
            protocolsAdoptedBySuper = fromMaybe emptySet $ fmap ciProtocols $ mbSuperInfo
            
                                              
data MangledSelector = MangledSelector {
            msSel :: Selector,
            msMangled :: String,
            msType :: HSelectorType
        }
        deriving (Eq)
msName = selName . msSel

prepareDeclarations :: BindingScript -> [HeaderInfo] -> IO PreparedDeclarations

prepareDeclarations bindingScript modules = do
    let allDecls = concatMap (\(HeaderInfo mod _ decls) -> map ((,) mod) decls) modules

        classes = mapMaybe classInfoForDeclaration $ allDecls
        
        classNames = [ (nameToUppercase name, (ClassTypeName, mod))
                     | (mod, SelectorList (Interface name _ _) _) <- allDecls ]
        (enumNamesAndLocations, enumDefinitions) = extractEnums bindingScript modules
        
        typeEnv = TypeEnvironment $ listToFM $
                  classNames ++ [ (name, (PlainTypeName, mod))
                                | (name, mod) <- enumNamesAndLocations
                                                 ++ bsAdditionalTypes bindingScript ]
                                                            
    print classNames
    putStrLn "collecting categories..."
    classHash <- HashTable.fromList HashTable.hashString classes
    mapM_ (updateClassInfoForCategory classHash) allDecls
    classes' <- HashTable.toList classHash
    
    putStrLn "cleaning class infos..."
    cleanClassInfoHash <- HashTable.new (==) HashTable.hashString
    mapM_ (cleanClassInfo cleanClassInfoHash classHash) (map fst classes')
    
    cleanClassInfos <- HashTable.toList cleanClassInfoHash
    
    let allInstanceSels :: [ (ClassInfo, [(MangledSelector, SelectorLocation)]) ]
        allInstanceSels = [ (ci, mangleSelectors False (ciName ci) (ciNewInstanceMethods ci))
                          | ci <- map snd cleanClassInfos ]
        allClassSels :: [ (ClassInfo, [(MangledSelector, SelectorLocation)]) ]
        allClassSels =  [ (ci, mangleSelectors True (ciName ci) (ciNewClassMethods ci))
                        | ci <- map snd cleanClassInfos ]
    
        externVarDeclarations = extractDecls varDecl
            where varDecl (ExternVar t n)
                    = do
                        ht <- getVariableType typeEnv t
                        return (ht, n, nameToLowercase n)
                    
                  varDecl _ = Nothing
                  
        externFunDeclarations = extractDecls funDecl
            where funDecl (ExternFun sel)
                    = do
                        typ <- getSelectorType PlainSelector typeEnv sel
                        return $ MangledSelector {
                                msSel = sel,
                                msMangled = nameToLowercase (selName sel),
                                msType = typ
                            }
                  funDecl _ = Nothing
                  
        extractDecls f = listToFM $
                         map (\(HeaderInfo mod _ decls) -> (mod, mapMaybe f decls)) $ 
                         modules
    
        mangleSelectors factory clsName sels =
            mapMaybe (\(sel, location) -> do {- Maybe -}
                    let name = selName sel
                        mapped = lookupFM (soNameMappings selectorOptions) name
                        mangled = case mapped of
                                    Just x -> x
                                    Nothing -> mangleSelectorName name
                        replacement = lookupFM (soChangedSelectors selectorOptions) name
                        sel' = case replacement of
                            Just x -> x
                            Nothing -> sel
                    
                    when (name `elementOf` soHiddenSelectors selectorOptions) $ Nothing
                    
                    let covariant = mangled `elementOf` soCovariantSelectors selectorOptions
                        kind | covariant && factory = CovariantInstanceSelector
                             | covariant = CovariantSelector
                             | "alloc" `isFirstWordOf` name = AllocSelector
                             | "init" `isFirstWordOf` name = InitSelector
                             | otherwise = PlainSelector
                        a `isFirstWordOf` b 
                            | length b > length a = (a `isPrefixOf` b)
                                                 && (not $ isLower (b !! length a))
                            | otherwise = a == b
                    
                    typ <- getSelectorType kind typeEnv sel'
                    return $ (MangledSelector {
                            msSel = sel',
                            msMangled = mangled,
                            msType = typ
                        }, location)
                ) $ fmToList sels
            where
                selectorOptions = getSelectorOptions bindingScript clsName
    
    return $ PreparedDeclarations {
                 pdCleanClassInfos = cleanClassInfos,
                 pdCleanClassInfoHash = cleanClassInfoHash,
                 pdAllInstanceSels = allInstanceSels,
                 pdAllClassSels = allClassSels,
                 pdEnumTypeDefinitions = enumDefinitions,
                 pdExternVarDeclarations = externVarDeclarations,
                 pdExternFunDeclarations = externFunDeclarations,
                 pdTypeEnvironment = typeEnv
             }
