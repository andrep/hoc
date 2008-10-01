module ShuffleInstances where

import Entities
import Progress

import Control.Monad.State
import Data.Maybe           ( fromMaybe )

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

expandProtocolRequirements :: EntityPile -> EntityPile
expandProtocolRequirements = execState $
    do
        entityPile <- get
        flip mapM_ (Map.elems $ localEntities entityPile) $
            \entity -> case eInfo entity of
                ProtocolAdoptionEntity -> do
                        mapM_ addMethod selectors
                        mapM_ addProto $ Set.toList $ Set.delete proto $ protos
                    where
                        ProtocolAdoptionName cls proto = eName entity
                        
                        (protos, selectors) = collect proto (Set.empty, [])
                        
                        collect proto (protos, sels)
                            | proto `Set.member` protos = (protos, sels)
                            | otherwise = foldr collect (Set.insert proto protos, 
                                                           selectors ++ sels)
                                                        superProtocols
                            where
                                ProtocolEntity superProtocols selectors
                                    = eInfo $ lookupEntity "expand" proto entityPile
                        
                        addMethod sel
                            = newEntity $ Entity {
                                eName = SelectorInstanceName cls sel False,
                                eHaskellName = BS.empty,
                                eAlternateHaskellNames = [],
                                eInfo = MethodEntity,
                                eModule = eModule entity,
                                eSrcPos = eSrcPos entity
                            }

                        addProto proto
                            = newEntity $ Entity {
                                eName = ProtocolAdoptionName cls proto,
                                eHaskellName = BS.empty,
                                eAlternateHaskellNames = [],
                                eInfo = ProtocolAdoptionEntity,
                                eModule = eModule entity,
                                eSrcPos = eSrcPos entity                                
                            }
                _ -> return ()



eliminateSubclassInstances :: ProgressReporter -> EntityPile -> EntityPile
eliminateSubclassInstances pr entityPile
    = -- (length $ show (instancesMap, clsTree)) `seq` 
      Map.size instancesMap `seq`
      Map.size clsTree `seq`
      (
        transformLocalEntities (Map.filter keepEntity) $
        reportProgressForPile pr entityPile
      )
    where
        keepEntity entity
            = case isInstance entity of
                Just (classID, adoptedID)
                    | any (\s -> adoptedID `Set.member` instances s)
                          (clsTree Map.! classID)
                    -> False
                _   -> True

        isInstance (Entity { eName = ProtocolAdoptionName classID protoID })
            = Just (classID, protoID)
        isInstance (Entity { eName = SelectorInstanceName classID selID isFactory })
            = Just (classID, selID)
        isInstance _ = Nothing

        instances :: EntityID -> Set.Set EntityID
        instances = fromMaybe Set.empty . flip Map.lookup instancesMap
                          
        instancesMap =
            Map.fromListWith Set.union
            [ (classID, Set.singleton adoptedID)
            | Just (classID, adoptedID)
                <- map isInstance $ map snd $
                   entityPileToList entityPile ]

            -- a "reversed tree" of class inheritance
            -- For every class, it contains the list of superclasses
            -- Tails are shared.
        clsTree :: Map.Map EntityID [EntityID]
        clsTree = Map.fromList $
                  [ (classID, maybe [] (\s -> s : clsTree Map.! s) mbSuper)
                  | (classID, Entity { eInfo = ClassEntity mbSuper } )
                    <- entityPileToList entityPile ]

