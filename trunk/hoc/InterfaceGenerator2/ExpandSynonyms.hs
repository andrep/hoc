{-# OPTIONS -fglasgow-exts #-}
module ExpandSynonyms where

import Language.Haskell.TH

expandSynonyms typ
    = typ >>= flip expandSynonyms1 []
    where
        expandSynonyms1 (AppT a b) pending
            = do
                b' <- expandSynonyms1 b []
                expandSynonyms1 a (b' : pending)
                
        expandSynonyms1 (ForallT vars ctx t) pending
            = do
                t' <- expandSynonyms1 t []
                return $ foldl AppT t' pending
                
        expandSynonyms1 (ConT n) pending
            = do
                info <- reify n
                case info of
                    TyConI (TySynD _ args body) ->
                            expandSynonyms1 (substTy taken body) rest
                        where
                            taken = zip args pending
                            rest = drop (length taken) pending
                    _ -> return $ foldl AppT (ConT n) pending
        expandSynonyms1 other pending
            = return $ foldl AppT other pending

        substTy mapping (ForallT names cxt t)
            = ForallT names cxt (substTy mapping' t)
            where mapping' = filter (not . (`elem` names) . fst) mapping
        substTy mapping (VarT name)
            = case lookup name mapping of
                Just t -> t
                Nothing -> VarT name
        substTy mapping (AppT a b)
            = AppT (substTy mapping a) (substTy mapping b)
        substTy _ other
            = other
            
