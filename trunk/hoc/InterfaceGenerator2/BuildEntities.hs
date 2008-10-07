module BuildEntities(
        renameToFramework,
        makeEntities,
        loadAdditionalCode
    ) where

import Entities
import Traversals
import BindingScript
import SyntaxTree
import SrcPos
import CTypeToHaskell
import Headers

import HOC.NameCaseChange
import HOC.SelectorNameMangling

import Control.Monad.State
import Data.Char        ( isUpper, isLower, isAlphaNum, toUpper )
import Data.List        ( groupBy, isPrefixOf )
import Data.Maybe       ( fromMaybe )
import System.Directory ( doesFileExist )
import System.FilePath  ( (</>) )

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

assertHaskellTypeName :: BS.ByteString -> BS.ByteString
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
                        eAlternateHaskellNames = [],
                        eInfo = AdditionalTypeEntity,
                        eModule = LocalModule $ BS.pack moduleName,
                        eSrcPos = AutoGeneratedPos
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
            
        makeSelectorEntity pos factory modName _clsID clsName sel
            = if hidden
                then return []
                else do
                    entity <- newEntity $ Entity {
                            eName = SelectorName $ BS.pack name,
                            eHaskellName = BS.pack mangled,
                            eAlternateHaskellNames = moreMangled,
                            eInfo = SelectorEntity (UnconvertedType (kind, sel')),
                            eModule = LocalModule modName,
                            eSrcPos = pos
                        }
                    return $ [(entity, factory)]
            where
                selectorOptions = getSelectorOptions bindingScript clsName
        
                name = selName sel
                mapped = Map.lookup name (soNameMappings selectorOptions)
                mangled = case mapped of
                            Just x -> x
                            Nothing -> mangleSelectorName name
                moreMangled = map BS.pack $ case mapped of
                    Just _ -> [mangleSelectorName name, mangleSelectorNameWithUnderscores name]
                    Nothing -> [mangleSelectorNameWithUnderscores name]
                            
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

        makeEntitiesForSelectorListItem modName clsID clsName (pos, InstanceMethod sel)
            = makeSelectorEntity pos False modName clsID clsName sel
        makeEntitiesForSelectorListItem modName clsID clsName (pos, ClassMethod sel)
            = makeSelectorEntity pos True modName clsID clsName sel
        makeEntitiesForSelectorListItem modName _clsID _clsName (pos, LocalDecl decl)
            = makeEntity modName (pos, decl) >> return []
        makeEntitiesForSelectorListItem modName clsID clsName (pos, PropertyDecl typ name attr)
            = do
                getter <- makeSelectorEntity pos False modName clsID clsName getterSel
                setter <- makeSelectorEntity pos False modName clsID clsName setterSel
                return (getter ++ setter)
            where
                getterName = head $ [ n | Getter n <- attr ] ++ [ name ]
                setterName = head $ [ n | Setter n <- attr ] ++
                                    [ "set" ++ toUpper (head name) : tail name ++ ":" ]
                getterSel = Selector getterName typ [] False
                setterSel = Selector setterName
                                     (CTSimple "void") [typ] False
        makeEntitiesForSelectorListItem _modName _clsID _clsName (pos, Required _)
            = return []
        
        makeSelectorEntities modName clsID clsName items
            = fmap concat $
              mapM (makeEntitiesForSelectorListItem modName clsID clsName) items
                
        makeSelectorInstance pos modName classEntity (selectorEntity, factory)
            = newEntity $ Entity {
                    eName = SelectorInstanceName classEntity selectorEntity factory,
                    eHaskellName = BS.empty,
                    eAlternateHaskellNames = [],
                    eInfo = MethodEntity,
                    eModule = LocalModule modName,
                    eSrcPos = pos
                }
        
        makeEntity modName (pos, SelectorList (Interface clsName mbSuper protocols) contents)
            | notHidden clsName
            = do
                classEntity <- newEntity $ Entity {
                        eName = CName $ BS.pack clsName,
                        eHaskellName = getName clsName (nameToUppercase clsName),
                        eAlternateHaskellNames = [],
                        eInfo = ClassEntity (fmap (DelayedClassLookup . BS.pack) mbSuper),
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }
                flip mapM_ protocols $ \protocol ->
                    newEntity $ Entity {
                            eName = ProtocolAdoptionName (DelayedClassLookup $ BS.pack clsName)
                                        (DelayedProtocolLookup $ BS.pack protocol),
                            eHaskellName = BS.empty,
                            eAlternateHaskellNames = [],
                            eInfo = ProtocolAdoptionEntity,
                            eModule = LocalModule modName,
                            eSrcPos = pos
                        }
                selectors <- makeSelectorEntities modName classEntity clsName contents
                mapM (makeSelectorInstance pos modName classEntity) selectors
                return ()
        makeEntity modName (pos, SelectorList (Category clsName _catName protocols) contents)
            = do
                let classEntity = DelayedClassLookup $ BS.pack clsName
                flip mapM_ protocols $ \protocol ->
                    newEntity $ Entity {
                            eName = ProtocolAdoptionName (DelayedClassLookup $ BS.pack clsName)
                                        (DelayedProtocolLookup $ BS.pack protocol),
                            eHaskellName = BS.empty,
                            eAlternateHaskellNames = [],
                            eInfo = ProtocolAdoptionEntity,
                            eModule = LocalModule modName,
                            eSrcPos = pos
                        }
                selectors <- makeSelectorEntities modName classEntity clsName contents
                mapM (makeSelectorInstance pos modName classEntity) selectors
                return ()
        makeEntity modName (pos, SelectorList (Protocol protoName protocols) contents)
            | notHidden protoName
            = mfix (\protocolEntity -> do
                selectors <- fmap (map fst) $ makeSelectorEntities modName
                                    protocolEntity (protoName ++ "Protocol") contents
                newEntity $ Entity {
                        eName = ProtocolName $ BS.pack protoName,
                        eHaskellName = getName protoName (nameToUppercase protoName ++ "Protocol"),
                        eAlternateHaskellNames = [],
                        eInfo = ProtocolEntity (map (DelayedProtocolLookup . BS.pack) protocols)
                                               selectors,
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }               
              ) >> return ()
        makeEntity modName (pos, Typedef (CTStruct n2 fields) name)
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToUppercase name),
                        eAlternateHaskellNames = [],
                        eInfo = StructEntity mbTag $ map (UnconvertedType . fst) fields,
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }
                return ()
            where
                mbTag = if n2 == "" then Nothing else Just n2
        makeEntity _modName (pos, Typedef (CTUnion _n2 _fields) _name)
            = return ()
        makeEntity modName (pos, Typedef (CTEnum _n2 vals) name)
            | notHidden name
            = makeEnum name pos modName vals
        makeEntity modName (pos, CTypeDecl (CTEnum name vals))
            | null name || notHidden name
            = (if null name then makeAnonymousEnum else makeEnum name) pos modName vals
        
        makeEntity modName (pos, Typedef ct name)
            | notHidden name
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToUppercase name),
                        eAlternateHaskellNames = [],
                        eInfo = TypeSynonymEntity (UnconvertedType ct),
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }
                return ()
        makeEntity modName (pos, ExternVar ct name)
            | notHidden name
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToLowercase name),
                        eAlternateHaskellNames = [],
                        eInfo = ExternVarEntity (UnconvertedType ct),
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }
                return ()
        makeEntity modName (pos, ExternFun sel)
            | notHidden name
            = do
                newEntity $ Entity {
                        eName = CName $ BS.pack name,
                        eHaskellName = getName name (nameToLowercase name),
                        eAlternateHaskellNames = [],
                        eInfo = ExternFunEntity (UnconvertedType (PlainSelector, sel)),
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }
                return ()
            where name = selName sel

        makeEntity _modName _ = return ()

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
                    
        makeEnum name pos modName values
            = case convertEnumEntities values of
                (True, values') -> do
                    newEntity $ Entity {
                            eName = CName $ BS.pack name,
                            eHaskellName = getName name (nameToUppercase name),
                            eAlternateHaskellNames = [],
                            eInfo = EnumEntity True values',
                            eModule = LocalModule modName,
                            eSrcPos = pos
                        }
                    return ()
                (False, values') -> do
                    newEntity $ Entity {
                            eName = Anonymous,
                            eHaskellName = BS.empty,
                            eAlternateHaskellNames = [],
                            eInfo = EnumEntity False values',
                            eModule = LocalModule modName,
                            eSrcPos = pos
                        }
                    newEntity $ Entity {
                            eName = CName $ BS.pack name,
                            eHaskellName = getName name (nameToUppercase name),
                            eAlternateHaskellNames = [],
                            eInfo = TypeSynonymEntity (UnconvertedType cTypeInt),
                            eModule = LocalModule modName,
                            eSrcPos = pos
                        }                    
                    return ()
        makeAnonymousEnum pos modName values
            = do
                let (complete, values') = convertEnumEntities values
                newEntity $ Entity {
                        eName = Anonymous,
                        eHaskellName = BS.empty,
                        eAlternateHaskellNames = [],
                        eInfo = EnumEntity complete values',
                        eModule = LocalModule modName,
                        eSrcPos = pos
                    }
                return ()

-- *****************************************************************************
-- pass 1.5: load additional code
-- *****************************************************************************

loadAdditionalCode :: String -> [String] -> EntityPile -> IO EntityPile
loadAdditionalCode additionalCodePath modNames entityPile
    = flip execStateT entityPile $ do
        flip mapM_ modNames $ \modName -> do
            let additionalCodeName
                    = additionalCodePath
                      </> map (\c -> if c == '.' then '/' else c) modName
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
                        eAlternateHaskellNames = [],
                        eInfo = AdditionalCodeEntity
                                    2
                                    exports
                                    imports2
                                    above,
                        eModule = LocalModule $ BS.pack modName,
                        eSrcPos = AutoGeneratedPos
                    }
                newEntity $ Entity {
                        eName = Anonymous,
                        eHaskellName = BS.empty,
                        eAlternateHaskellNames = [],
                        eInfo = AdditionalCodeEntity
                                    9
                                    []
                                    imports1
                                    below,
                        eModule = LocalModule $ BS.pack modName,
                        eSrcPos = AutoGeneratedPos
                    }
                return ()
