module ResolveAndZap where

import Entities
import Traversals
import CTypeToHaskell
import Messages
import Progress

import Control.Monad.State
import Data.Maybe       ( fromMaybe, mapMaybe, catMaybes )
import Text.PrettyPrint.HughesPJ

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

-- *****************************************************************************
-- pass 2: resolve references
-- *****************************************************************************

resolveReferences :: EntityPile -> EntityPile
resolveReferences entityPile
    = transformLocalEntities (Map.map $ mapEntityIDs resolve) entityPile
    where
        entitiesByName = makeEntityPileIndex eName entityPile
        lookupByName wrapper name
            = fromMaybe (error $ "unresolved reference: " ++ show (wrapper name)) $
              Map.lookup (wrapper name) entitiesByName
              
        resolve (DelayedClassLookup name) = lookupByName CName name
        resolve (DelayedProtocolLookup name) = lookupByName ProtocolName name
        resolve other = other

-- *****************************************************************************
-- pass X: resolve types
-- *****************************************************************************

resolveTypes :: EntityPile -> EntityPile
resolveTypes entityPile
    = transformLocalEntities (Map.map $ mapTypes resolveVal resolveSel) entityPile
    where
        resolveSel :: HaskellSelectorType -> HaskellSelectorType
        resolveSel uct@(UnconvertedType (kind, sel))
            = case getSelectorType kind typeEnvironment sel of
                Just ht -> ConvertedType ht (resolveMentioned $ mentionedTypes ht)
                Nothing -> uct
        resolveSel x = x
                
        resolveVal :: HaskellValueType -> HaskellValueType        
        resolveVal uct@(UnconvertedType ctype)
            = case getVariableType typeEnvironment ctype of
                Just ht -> ConvertedType ht (resolveMentioned $ varMentionedTypes ht)
                Nothing -> uct
        resolveVal x = x
        
        resolveMentioned = map (\n -> fromMaybe (error $ "resolveMentioned " ++ show n) $
                                        Map.lookup (CName $ BS.pack n) byName)

        byName = makeEntityPileIndex eName entityPile

        typeEnvironment = TypeEnvironment $ Map.fromList $
                            mapMaybe typeEnvironmentEntryForEntity $
                            map snd $
                            entityPileToList entityPile
    
        typeEnvironmentEntryForEntity e
            = case eName e of
                CName n -> case kind of
                            Just k -> return (BS.unpack n, (k, error "TypeEnvironment.definedIn"))
                            Nothing -> Nothing
                _ -> Nothing
            where
                kind = case eInfo e of
                    TypeSynonymEntity _ -> Just PlainTypeName
                    AdditionalTypeEntity -> Just PlainTypeName
                    EnumEntity _ _ -> Just PlainTypeName
                    ClassEntity _ -> Just ClassTypeName
                    _ -> Nothing

zapAndReportWith :: ((EntityID, Entity) -> Messages (EntityID, Entity))
                 -> ProgressReporter
                 -> EntityPile -> Messages EntityPile
zapAndReportWith worker progress entityPile
    = do
        message $ text "Zapping..."
        (entities', messages)
            <- listenMessages $
               fmap (Map.fromList . catMaybes) $
               flip mapM (map (monitor1 progress 0) $ Map.toList $ localEntities entityPile) $
            \(entityID, entity) ->
                case runMessages $ worker (entityID, entity) of
                    (x, [])
                        -> return $ Just x
                    (_, messages)
                        -> do
                            message (text "Skipping"
                                        <+> (text.show) entityID
                                        <+> parens (text $ show $ eName entity)
                                        $+$ nest 4 (vcat messages))
                            return Nothing
        let pile' = replaceLocalEntities entities' entityPile
        case messages of
            [] -> return $ monitor1 progress 1 $ pile'
            _  -> zapAndReportBrokenReferences progress pile'
    where            
        
zapAndReportFailedTypes :: ProgressReporter -> EntityPile -> Messages EntityPile    
zapAndReportFailedTypes progress entityPile
    = zapAndReportWith worker progress entityPile
    where
        worker x = 
            (foreachTypeInEntity reportUnconvertedType reportUnconvertedType . snd) x
            >> return x
        
        reportUnconvertedType t@(UnconvertedType ctype)
            = message $ text "Coudn't convert type" <+> text (show ctype)
        reportUnconvertedType t
            = return ()

zapAndReportBrokenReferences :: ProgressReporter -> EntityPile -> Messages EntityPile
zapAndReportBrokenReferences progress entityPile
    = zapAndReportWith worker progress entityPile
    where
        cutDownProtocol entity
            = case eInfo entity of
                    ProtocolEntity supers selectors ->
                        entity { eInfo
                            = ProtocolEntity supers
                                (filter (flip hasEntity entityPile) selectors) }
                    _ -> entity

        worker (entityID, entity)
            = do
                let entity' = cutDownProtocol entity
                foreachIDInEntity reportBrokenRef entity'
                return (entityID, entity')
        
        reportBrokenRef eid
            = unless (hasEntity eid entityPile) $
                    message $ text (show eid) <+> text "has been deleted."
