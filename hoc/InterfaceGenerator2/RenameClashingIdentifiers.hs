module RenameClashingIdentifiers( renameClashingIdentifiers ) where

import Entities
import qualified Data.Map as Map
import Data.List( sort, sortBy, groupBy, nub )

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS

renameClashingIdentifiers :: EntityPile -> EntityPile

renameClashingIdentifiers ep
    = ep { epEntities = Map.fromList $
                        concatMap handleName $
                        groupedByName }
    where
        groupedByName :: [ (ByteString, [ (EntityID, Entity) ]) ]
        groupedByName
            = Map.toList $ Map.fromListWith (++) $
              [ ( eHaskellName entity, [(entityID, entity)] )
              | (entityID, entity) <- Map.toList $ epEntities ep ]
        
        handleName :: (ByteString, [ (EntityID, Entity) ]) -> [ (EntityID, Entity) ]
        handleName (hName, entities)
            | BS.null hName
            = entities
            | null clashes
            = entities
            | otherwise
            = concat $ zipWith renameEntities (map (map snd) groupedEntities) $ head possibleCombos
            where
                groupedEntities =
                    groupByFst $
                    sortBy (\a b -> compare (fst a) (fst b))
                    [ (originalEntityID e , e) | e <- entities ]
                                    
                names = map (possibleNamesFor . head) groupedEntities where
                    possibleNamesFor (LocalEntity _, (_, e))
                        = eHaskellName e : eAlternateHaskellNames e
                        ++ [ eHaskellName e `BS.append` BS.pack ("_" ++ show i) | i <- [1..] ]
                    possibleNamesFor (_, (_, e))
                        = [eHaskellName e]

                possibleCombos = filter checkCombo $ nameCombinations names

                clashes  :: [ [Int] ]
                clashes = 
                    filter ( (> 1) . length ) $
                    map nub $
                    map (map snd) $ groupByFst $ sort $
                        [ (eModule e, index)
                        | (index, entities) <- zip [0..] groupedEntities,
                          (_, (eid, e)) <- entities ]
                
                checkCombo newNames
                    = all checkClash clashes
                    where
                        checkClash clash = nub toBeTested == toBeTested
                            where toBeTested = extract clash newNames
                    
                        extract indices xs = map (xs!!) indices
                                      
                renameEntity (entityID, entity) newName
                    = (entityID, entity { eHaskellName = newName })
                renameEntities entities newName
                    = map (flip renameEntity newName) entities

        originalEntityID (_, Entity { eInfo = ReexportEntity entityID' })
            = originalEntityID (entityID', lookupEntity "originalEntityID" entityID' ep)
        originalEntityID (entityID, entity)
            = entityID
        
        groupByFst :: Eq a => [(a,b)] -> [[(a,b)]]
        groupByFst = groupBy (\a b -> fst a == fst b)
        
nameCombinations names = concat $ takeWhile (not . null) $ map (f names) [0..]
    where
        f [] i = return []
        f [ns] i = do
            lastName <- take 1 $ drop i ns
            return [lastName]
        f (ns:nss) i = do 
            (chosenIndex, chosenName) <- zip [0..i] ns
            moreChosenNames <- f nss (i - chosenIndex)
            return (chosenName : moreChosenNames)
            

