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

import HOC.SelectorNameMangling(mangleSelectorName)

import Control.Monad(when)
import Data.Set
import Data.FiniteMap
import qualified Data.HashTable as HashTable
import Data.Maybe(maybeToList, fromMaybe, mapMaybe)
import Data.List(partition)

data PreparedDeclarations = PreparedDeclarations {
        pdModuleNames :: [ModuleName],
        pdCleanClassInfos :: [(String, ClassInfo)],
        pdCleanClassInfoHash :: HashTable.HashTable String ClassInfo, {- used read only -}
        pdAllInstanceSels :: [(ClassInfo, [(MangledSelector, SelectorLocation)])],
        pdAllClassSels :: [(ClassInfo, [(MangledSelector, SelectorLocation)])]
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
    
instance (Show key, Show elem) => Show (FiniteMap key elem) where
    show = show . fmToList
instance (Show elem) => Show (Set elem) where
    show = show . setToList
    
classInfoForDeclaration (moduleName, SelectorList (Interface name super protocols) methods) =
    Just $ (name, ClassInfo {
        ciProtocol = False,
        ciName = name,
        ciSuper = super,
        ciProtocols = mkSet protocols,
        ciDefinedIn = moduleName,
        ciInstanceMethods = listToFM [ (sel, SelectorLocation moduleName moduleName)
                                     | InstanceMethod sel <- methods ],
        ciClassMethods = listToFM [ (sel, SelectorLocation moduleName moduleName)
                                  | ClassMethod sel <- methods ],
        ciNewProtocols = undefined,
        ciNewInstanceMethods = undefined,
        ciNewClassMethods = undefined
    })
classInfoForDeclaration (moduleName, SelectorList (Protocol name protocols) methods) =
    Just $ (name ++ "Protocol", ClassInfo {
        ciProtocol = True,
        ciName = name ++ "Protocol",
        ciSuper = Nothing,
        ciProtocols = mkSet protocols,
        ciDefinedIn = moduleName,
        ciInstanceMethods = listToFM [ (sel, SelectorLocation moduleName cantHappen)
                                     | InstanceMethod sel <- methods ],
        ciClassMethods = listToFM [ (sel, SelectorLocation moduleName cantHappen)
                                  | ClassMethod sel <- methods ],
        ciNewProtocols = undefined,
        ciNewInstanceMethods = undefined,
        ciNewClassMethods = undefined
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
                -- putStrLn $ "<" ++ name
                Just ci <- HashTable.lookup inInfos name
                (mbSuper, protocols, recheck) <- cleanSuper ci
                if recheck
                    then cleanClassInfo outInfos inInfos name
                    else do
                        -- putStrLn name
                        HashTable.insert outInfos name
                                         (cleanClassInfo' ci mbSuper protocols)
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
                                              -> SelectorLocation def inst)
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
        classNames = mkSet [ name | (mod, SelectorList (Interface name _ _) _) <- allDecls ]
        moduleNames = map (\(HeaderInfo name _ _) -> name) $ modules

        classes = mapMaybe classInfoForDeclaration $ allDecls
        
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
        allClassSels =    [ (ci, mangleSelectors True (ciName ci) (ciNewClassMethods ci))
                       | ci <- map snd cleanClassInfos ]
    
        mangleSelectors factory clsName sels =
            mapMaybe (\(sel, location) -> do
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
                    
                    typ <- if mangled `elementOf` soCovariantSelectors selectorOptions
                        then getCovariantSelectorType factory classNames sel
                        else getSelectorType classNames sel
                    return $ (MangledSelector {
                            msSel = sel',
                            msMangled = mangled,
                            msType = typ
                        }, location)
                ) $ fmToList sels
            where
                selectorOptions = getSelectorOptions bindingScript clsName
    
    return $ PreparedDeclarations {
                 pdModuleNames = moduleNames,
                 pdCleanClassInfos = cleanClassInfos,
                 pdCleanClassInfoHash = cleanClassInfoHash,
                 pdAllInstanceSels = allInstanceSels,
                 pdAllClassSels = allClassSels
             }
