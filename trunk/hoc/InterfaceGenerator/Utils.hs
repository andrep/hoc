module Utils(
        mapFst,
        mapSnd,
        groupByFirst
    ) where
    
import Data.List(groupBy, sortBy)
    
groupByFirst :: (Ord a, Eq a) => [(a,b)] -> [(a,[b])]
groupByFirst xs = map (\xys@((x,y):_) -> (x,map snd xys)) $
                  groupBy eqFirst $
                  sortBy compareFirst xs
    where
        compareFirst (a,b) (c,d) = compare a c
        eqFirst (a,b) (c,d) = a == c

mapFst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapFst f = map (\(a,b) -> (f a, b))

mapSnd:: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd f = map (\(a,b) -> (a, f b))

