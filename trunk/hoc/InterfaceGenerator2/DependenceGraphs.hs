module DependenceGraphs(
        entitiesRequiredByEntity,
        RGr,
        makeEntityGraph,
        makeModuleGraph,
        makeModuleDAG,
        topsortEntities,
        minimizeSourceImports,
        isSourceImport
    ) where

import Entities
import Traversals           ( mentionedEntityIDs )

import Control.Monad        ( guard )
import Data.Graph.Inductive
import Data.List            ( groupBy, sortBy, mapAccumL )
import Data.Maybe           ( fromMaybe, maybeToList, catMaybes, mapMaybe )
import qualified Data.Map as Map
import qualified Data.Set as Set

entitiesRequiredByEntity :: Entity -> [EntityID]

entitiesRequiredByEntity e
    = mentionedEntityIDs e

type RGr a b = (Gr a b, Map.Map a Node)

makeEntityGraph :: EntityPile -> RGr EntityID Bool
makeEntityGraph entityPile
    = (gr, entityToNode)
    where
        entities = localEntities entityPile
        entityToNode = Map.fromList $ zip (Map.keys entities) [1..]
        gr = mkGraph (zip [1..] (Map.keys entities)) $
                do {- list -}
                    (fromEntityID, e) <- Map.toList entities
                    let from = entityToNode Map.! fromEntityID
                        weak = case eInfo e of
                                ProtocolEntity _ _ -> True
                                _ -> False
                    toEntityID <- entitiesRequiredByEntity e
                    
                    case toEntityID of
                        LocalEntity _ ->
                            return (from, entityToNode Map.! toEntityID,
                                    weak)
                        FrameworkEntity _ _ ->
                            []

makeModuleGraph :: EntityPile -> RGr Module Bool
makeModuleGraph entityPile
    = (gr, moduleToNode)                
    where
        byModule = makeEntityPileLocalMultiIndex eModule entityPile
        moduleToNode = Map.fromList $ zip (Map.keys byModule) [1..]
        gr = mkGraph (zip [1..] (Map.keys byModule)) $
            do  {- list -}
                (fromMod, entitySet) <- Map.toList byModule
                
                (toMod, isSource) <- Map.toList $ Map.fromListWith (&&) $ do {- list -}
                    importedEntityID <-
                        Set.toList $ Set.unions $ map Set.fromList $
                        map (entitiesRequiredByEntity . 
                             flip (lookupEntity "makeModuleGraph1") entityPile) $
                        Set.toList entitySet
                    
                    let importedEntity = lookupEntity "makeModuleGraph2" importedEntityID entityPile
                    
                    guard (eModule importedEntity /= fromMod)
                    
                    case eInfo importedEntity of
                        ClassEntity _ -> return (eModule importedEntity, True)
                        _             -> return (eModule importedEntity, False)
                
                fromNode <- maybeToList $ Map.lookup fromMod moduleToNode
                toNode   <- maybeToList $ Map.lookup  toMod  moduleToNode
                return (fromNode, toNode, isSource)

makeModuleDAG :: EntityPile -> RGr Module ()
makeModuleDAG entityPile
    = (emap (const ()) $ elfilter not gr, moduleToNode)
    where
        (gr, moduleToNode) = makeModuleGraph entityPile

topsortEntities :: EntityPile -> [(EntityID, Entity)] -> [(EntityID, Entity)]
topsortEntities entityPile idsAndEntities
    =   concatMap topsortGroup $
        groupBy (categorize (==)) $
        sortBy (categorize compare) $
        idsAndEntities
    where
        categorize f a b = f (category a) (category b)
    
        category (_, Entity { eInfo = ClassEntity _} ) = 1
        category (_, Entity { eInfo = MethodEntity } ) = 10
        category (_, Entity { eInfo = ProtocolAdoptionEntity } ) = 10
        category (_, Entity { eInfo = AdditionalCodeEntity c _ _ _ } ) = c
        category _ = 5
    
        topsortGroup idsAndEntities
            = map (\eid -> (eid, lookupEntity "topsortEntities" eid entityPile)) $
              mapMaybe (lab gr) $ reverse $ topsort gr 
            where
                (ids, entities) = unzip idsAndEntities
                nodes = zip [1..] ids
                rnodes = Map.fromList $ zip ids [1..]
                edges = [ (from, to, ())
                        | (from, fromEntity) <- zip [1..] entities
                        , toID <- entitiesRequiredByEntity fromEntity
                        , to <- maybeToList $ Map.lookup toID rnodes  ]
                gr = mkGraph nodes edges :: Gr EntityID ()
        
minimizeSourceImports :: RGr Module Bool -> RGr Module Bool
minimizeSourceImports (gr, rmap)
    = (gr', rmap)
    where
        gr' = flip insEdges gr $ catMaybes $ snd $
              mapAccumL testEdge dag (labEdges gr)
        dag = emap (const ()) $ elfilter not gr
        
        testEdge dag (n1,n2,False) = (dag, Nothing)
        testEdge dag (n1,n2,True)
            | n1 `elem` reachable n2 dag
            = (dag, Just (n1,n2,True))
            | otherwise
            = (insEdge (n1,n2,()) dag, Just (n1,n2,False))
        
lookupEdgeInRGr :: Ord a => a -> a -> RGr a b -> Maybe b
lookupEdgeInRGr a b (gr, rmap)
    = lookup n2 $ lsuc gr n1
    where
        n1 = rmap Map.! a
        n2 = rmap Map.! b

isSourceImport :: Module -> Module -> RGr Module Bool -> Bool
isSourceImport x y
    = fromMaybe (error "isSourceImport: no import at all!")
    . lookupEdgeInRGr x y
