module DuplicateEntities ( combineDulicateEntities ) where

import Entities
import Traversals       ( mapEntityIDsInEntity )
import DependenceGraphs ( makeModuleDAG )

import Data.Graph.Inductive
import Data.List    ( partition, sortBy )
import Data.Maybe   ( fromMaybe, mapMaybe )
import qualified Data.Map as Map
import qualified Data.Set as Set


findDuplicateEntities :: EntityPile -> [[EntityID]]
findDuplicateEntities entityPile
    =   filter (not . null . tail) $
        map Set.toList $
        Map.elems $
        Map.unionWith Set.union localThings frameworkThings
    where
        localThings = Map.fromListWith Set.union $
                [ ((eHaskellName e, eName e, eInfo e), Set.singleton ei)
                | (ei, e) <- Map.toList $ localEntities entityPile,
                  interesting e ]
        frameworkThings = Map.fromList
                [ ((eHaskellName e, eName e, eInfo e), Set.singleton ei)
                | (ei, e) <- Map.toList $ frameworkEntities entityPile,
                  interesting e ]
    
        interesting (Entity { eInfo = SelectorEntity _}) = True
        interesting (Entity { eInfo = MethodEntity }) = True
        interesting (Entity { eInfo = ProtocolAdoptionEntity }) = True
        interesting _ = False

pickMasterEntity :: Map.Map Module Int -> EntityPile -> [EntityID] -> (EntityID, [EntityID])

pickMasterEntity topsorted entityPile duplicateEntities
    = case partition isFrameworkEntity duplicateEntities of
        ([fw], others) -> (fw, others)
        ([], _) -> case sortBy compareEntities duplicateEntities of
                    (x:xs) -> (x, xs)
    where
        isFrameworkEntity (FrameworkEntity _ _) = True
        isFrameworkEntity _ = False

        compareEntities a b
            = compare (position a) (position b)
            where position x = topsorted Map.! eModule (lookupEntity "pickMaster" x entityPile)
            
reexportEntities :: Map.Map EntityID EntityID -> EntityPile -> EntityPile
reexportEntities remappings pile
    = 
        transformLocalEntities (Map.fromList . mapMaybe handleEntity . Map.toList) pile
    where        -- 
        handleEntity (eid, e)
            = case Map.lookup eid remappings of
                    Nothing -> Just (eid, mapEntityIDsInEntity redirect $ e)
                    Just eid' 
                        -> Just (eid, mapEntityIDsInEntity redirect $
                                      e { eInfo = ReexportEntity eid' })
                        
        redirect eid = fromMaybe eid $ Map.lookup eid remappings

combineDulicateEntities :: EntityPile -> EntityPile
combineDulicateEntities entityPile
    = resolve entityPile
    where
        (dag, _) = makeModuleDAG entityPile
        topsortedList = reverse $ mapMaybe (lab dag) $ topsort dag
        topsortedMap = Map.fromList $ zip topsortedList [1..]
        
        resolve entityPile
            | null dups = entityPile
            | otherwise = resolve $ reexportEntities remappings entityPile
            where
                dups = findDuplicateEntities entityPile
                remappings
                    = Map.fromList $ 
                      do {- list -}
                        group <- dups
                        let (master, reexports)
                                = pickMasterEntity topsortedMap entityPile group
                        reexport <- reexports
                        return (reexport, master)
