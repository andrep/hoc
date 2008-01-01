module BuildEntities(
        renameToFramework,
        makeEntities,
        loadAdditionalCode
    ) where

import Entities
import Traversals
import BindingScript
import SyntaxTree
import CTypeToHaskell
import Headers

import HOC.NameCaseChange
import HOC.SelectorNameMangling

import Control.Monad.State
import Data.Char        ( isUpper, isLower, isAlphaNum )
import Data.List        ( groupBy, isPrefixOf )
import Data.Maybe       ( fromMaybe, catMaybes )
import System.Directory ( doesFileExist )

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set


renameToFramework :: EntityMap -> Framework -> EntityMap
renameToFramework eMap framework
    = Map.mapKeys processID $
      Map.map (mapEntityIDsAndModules processID processModule) $
      eMap
    where
        processID (LocalEntity i) = FrameworkEntity framework i
        processID x = x
        
        processModule (LocalModule m) = FrameworkModule framework m
        processModule x = x

-- *****************************************************************************
-- pass 1: build entities, convert types & rename
-- *****************************************************************************

makeEntities :: BindingScript -> [HeaderInfo] -> EntityPile -> EntityPile

assertHaskellTypeName xs
    | not (BS.null xs)
    && isUpper x && BS.all (\c -> isAlphaNum c || c `elem` "_'") xs
    = xs
    where
        x = BS.head xs

-- loadedHeaders
makeEntities bindingScript headers importedEntities
    = flip execState importedEntities $ do
            sequence_ [
                    newEntity $ Entity {
                        eName = CName $ BS.pack typeName,
                        eHaskellName = assertHaskellTypeName $ BS.pack typeName,
                        eInfo = AdditionalTypeEntity,
                        eModule = LocalModule $ BS.pack moduleName
                    }
                    | (typeName, moduleName) <- bsAdditionalTypes bindingScript,
                    BS.pack moduleName `Set.member` modNames
                ]
            mapM_ makeEntitiesForHeader headers
    where
        modNames = Set.fromList [ modName | HeaderInfo modName _ _ <- headers ]
        makeEntitiesForHeader (HeaderInfo modName _ decl)
            = mapM_ (makeEntity modName) decl
        
        notHidden :: String -> Bool
        notHidden = not . (`Set.member` bsHidden bindingScript)
        
        getName :: String -> String -> BS.ByteString
        getName cname defaultHName
            = BS.pack $ fromMaybe defaultHName $
              Map.lookup cname (bsNameMappings bindingScript)
                
        
            -- HACK: for covariant selectors, there is a difference
            --       between factory methods and instance methods.
            --       This is bad, because it should really be the same selector
            --       in both cases, and we aren't equipped to deal with duplicates.
            -- Workaround: If there is both an instance method and a class method of the
            --             same name, don't use covariant.
            
        makeSelectorEntity factory modName clsID clsName sel
            = if hidden
                then return Nothing
                else do
                    entity <- newEntity $ Entity {
                            eName = SelectorName $ BS.pack name,
                            eHaskellName = BS.pack mangled,
                            eInfo = SelectorEntity (UnconvertedType (kind, sel')),
                            eModule = LocalModule modName
                        }
                    return $ Just (entity, factory)
            where
                selectorOptions = getSelectorOptions bindingScript clsName
        
                name = selName sel
                mapped = Map.lookup name (soNameMappings selectorOptions)
                mangled = case mapped of
                            Just x -> x
                            Nothing -> mangleSelectorName name
                replacement = Map.lookup name (soChangedSelectors selectorOptions)
                sel' = case replacement of
                    Just x -> x
                    Nothing -> sel
                    
                hidden = name `Set.member` soHiddenSelectors selectorOptions
                    
                covariant = mangled `Set.member` soCovariantSelectors selectorOptions
                kind | covariant && factory = CovariantInstanceSelector
                     | covariant = CovariantSelector
                     | "alloc" `isFirstWordOf` name = AllocSelector
                     | "init" `isFirstWordOf` name = InitSelector
                     | otherwise = PlainSelector
                a `isFirstWordOf` b 
                    | length b > length a = (a `isPrefixOf` b)
                                         && (not $ isLower (b !! length a))
                    | otherwise = a == b

        makeEntitiesForSelectorListItem modName clsID clsName (InstanceMethod sel)
            = makeSelectorEntity False modName clsID clsName sel
        makeEntitiesForSelectorListItem modName clsID clsName (ClassMethod sel)
            = makeSelectorEntity True modName clsID clsName sel
        makeEntitiesForSelectorListItem modName clsID clsName (LocalDecl decl)
            = makeEntity modName decl >> return Nothing
        makeEntitiesForSelectorListItem modName clsID clsName PropertyDecl
            = return Nothing
        makeEntitiesForSelectorListItem modName clsID clsName (Required _)
            = return Nothing
        
        makeSelectorEntities modName clsID clsName items
            = fmap catMaybes $
              mapM (makeEntitiesForSelectorListItem modName clsID clsName) items
                
        makeSelectorInstance modName classEntity (selectorEntity, factory)
            = newEntity $ Entity {
                    eName = SelectorInstanceName classEntity selectorEntity factory,
                    eHaskellName = BS.empty,
                    eInfo = MethodEntity,
                    eModule = LocalModule modName
                }
        
        makeEntity modName (SelectorList (Interface clsName mbSuper protocols) contents)
            | notHidden clsName
            = do
                classEntity <- newEntity $ Entity {
                        eName = CName $ BS.pack clsName,
                        eHaskellName = getName clsName (nameToUppercase clsName),
                        eInfo = ClassEntity (fmap (DelayedClassLookup . BS.pack) mbSuper),
                        eModule = LocalModule modName
                    }
                flip mapM_ protocols $ \protocol ->
                    newEntity $ Entity {
                            eName = ProtocolAdoptionName (DelayedClassLookup $ BS.pack clsName)
                                        (DelayedProtocolLookup $ BS.pack protocol),
                            eHaskellName = BS.empty,
                            eInfo = ProtocolAdoptionEntity,
                            eModule = LocalModule modName
                        }
                selectors <- makeSelectorEntities modName classEntity clsName contents
                mapM (makeSelectorInstance modName classEntity) selectors
                return ()
        makeEntity modName (SelectorList (Category clsName _catName protocols) contents)
            = do
                let classEntity = DelayedClassLookup $ BS.pack clsName
                flip mapM_ protocols $ \protocol ->
                    newEntity $ Entity {
                            eName = ProtocolAdoptionName (DelayedClassLookup $ BS.pack clsName)
                                        (DelayedProtocolLookup $ BS.pack protocol),
                            eHaskellName = BS.empty,
                            eInfo = ProtocolAdoptionEntity,
                            eModule = LocalModule modName
                        }
                selectors <- makeSelectorEntities modName classEntity clsName contents
                mapM (makeSelectorInstance modName classEntity) selectors
                return ()
        makeEntity modName (SelectorList (Protocol protoName protocols) contents)
            | notHidden protoName
            = mfix (\protocolEntity -> do
                selectors <- fmap (map fst) $ makeSelectorEntities modName
                                    protocolEntity (protoName ++ "Protocol") contents
                newEntity $ Entity {
                        eName = ProtocolName $ BS.pack protoName,
                        eHaskellName = getName protoName (nameToUppercase protoName ++ "Protocol"),
                        eInfo = ProtocolEntity (map (DelayedProtocolLookup . BS.pack) protocols)
                                               selectors,
                        eModule = LocalModule modName
                    }               
              ) >> return ()
        makeEntity modName (Typedef (CTStruct n2 fields) name)
            = return ()
        makeEntity modName (Typedef (CTUnion n2 fields) name)
            = return ()
        makeEntity modName (Typedef (CTEnum n2 vals) name)
            | notHidden name
            = makeEnum name modName vals
              -- makeAnonymousEnum modName vals -- ### HACK for 10.5: ignore enum names
        makeEntity modName (CTypeDecl (CTEnum name vals))
            | null name || notHidden name
            = (if null name {- || True {- ### see above -}-} then makeAnonymousEnum else makeEnum name) modName vals
        
        makeEntity modName (Typedef ct name)
            | notHidden name
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToUppercase name),
                        eInfo = TypeSynonymEntity (UnconvertedType ct),
                        eModule = LocalModule modName
                    }
                return ()
        makeEntity modName (ExternVar ct name)
            | notHidden name
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToLowercase name),
                        eInfo = ExternVarEntity (UnconvertedType ct),
                        eModule = LocalModule modName
                    }
                return ()
        makeEntity modName (ExternFun sel)
            | notHidden name
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToLowercase name),
                        eInfo = ExternFunEntity (UnconvertedType (PlainSelector, sel)),
                        eModule = LocalModule modName
                    }
                return ()
            where name = selName sel

        makeEntity modName _ = return ()

        convertEnumEntities :: [(String, EnumValue)]
                            -> (Bool, [(BS.ByteString, Integer)])
        convertEnumEntities values
            = (length converted == length values, converted)
            where
                converted = convert (Just 0) values
            
                convert _ ((name, GivenValue n) : xs)
                    = (BS.pack name, n) : convert (Just (n+1)) xs
                convert (Just n) ((name, NextValue) : xs)
                    = (BS.pack name, n) : convert (Just (n+1)) xs
                convert _ (_ : xs)
                    = convert Nothing xs
                convert _ [] = []
                    
        makeEnum name modName values
            = case convertEnumEntities values of
                (True, values') -> do
                    newEntity $ Entity {
                            eName = CName $ BS.pack name,
                            eHaskellName = getName name (nameToUppercase name),
                            eInfo = EnumEntity True values',
                            eModule = LocalModule modName
                        }
                    return ()
                (False, values') -> do
                    newEntity $ Entity {
                            eName = Anonymous,
                            eHaskellName = BS.empty,
                            eInfo = EnumEntity False values',
                            eModule = LocalModule modName
                        }
                    newEntity $ Entity {
                            eName = CName $ BS.pack name,
                            eHaskellName = getName name (nameToUppercase name),
                            eInfo = TypeSynonymEntity (UnconvertedType cTypeInt),
                            eModule = LocalModule modName
                        }                    
                    return ()
        makeAnonymousEnum modName values
            = do
                let (complete, values') = convertEnumEntities values
                newEntity $ Entity {
                        eName = Anonymous,
                        eHaskellName = BS.empty,
                        eInfo = EnumEntity complete values',
                        eModule = LocalModule modName
                    }
                return ()

-- *****************************************************************************
-- pass 1.5: load additional code
-- *****************************************************************************

loadAdditionalCode :: String -> [String] -> EntityPile -> IO EntityPile
loadAdditionalCode additionalCodePath0 modNames entityPile
    = flip execStateT entityPile $ do
        flip mapM_ modNames $ \modName -> do
            let additionalCodeName
                    = additionalCodePath -- ###
                      ++ map (\c -> if c == '.' then '/' else c) modName
                      ++ ".hs"
                      
            exists <- lift $ doesFileExist additionalCodeName
            when exists $ do
                additional <- lift $ BS.readFile additionalCodeName
                let additionalLines = BS.lines additional
                    separator = BS.pack "-- CUT HERE"

                    [imports1, below, imports2, above]
                        = map BS.unlines $
                          take 4 $
                          (++ repeat []) $
                          filter ((/= separator) . head) $
                          groupBy (eqBy (== separator)) additionalLines
                          
                    eqBy f a b = f a == f b
                    exportKey = BS.pack "--X "
                    exports = map (BS.drop 4) $
                              filter (exportKey `BS.isPrefixOf`) additionalLines

                newEntity $ Entity {
                        eName = Anonymous,
                        eHaskellName = BS.empty,
                        eInfo = AdditionalCodeEntity
                                    2
                                    exports
                                    imports2
                                    above,
                        eModule = LocalModule $ BS.pack modName
                    }
                newEntity $ Entity {
                        eName = Anonymous,
                        eHaskellName = BS.empty,
                        eInfo = AdditionalCodeEntity
                                    9
                                    []
                                    imports1
                                    below,
                        eModule = LocalModule $ BS.pack modName
                    }
                return ()
    where
        additionalCodePath
            | last additionalCodePath0 == '/' = additionalCodePath0
            | otherwise = additionalCodePath0 ++ "/"
