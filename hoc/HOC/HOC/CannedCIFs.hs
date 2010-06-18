{-# LANGUAGE TemplateHaskell #-}
module HOC.CannedCIFs where

import HOC.Base         ( SEL )
import HOC.Arguments    ( getCifForSelector )
import HOC.ID           ( ID )
import HOC.TH           ( fromSameModuleAs_v )
-- import HOC.THDebug

import Data.List        ( intersperse )
import Data.Maybe       ( catMaybes, fromMaybe )
import Data.Word        ( Word )
import Foreign          ( Ptr )
import Foreign.C
import Language.Haskell.TH

import Control.Arrow

-- removes all foralls (leaving in type variables) and de-sugars all type 
-- synonyms.
expandSynonyms :: Type -> Q Type
expandSynonyms typ = do
    appTargs <- fromAppT typ [] >>= stripForalls >>= applySynonyms
    return $ foldl1 AppT appTargs
    where
        -- turn a series of AppT arguments to a list recursively expanding the 
        -- arguments as we go.
        -- Notice that if a and b, after type synonym expansion consist of 
        -- AppTs, we continue to expand them, so in a certain sense, this 
        -- expansion "flattens" our AppTs
        fromAppT :: Type -> [Type] -> Q [Type]
        fromAppT (AppT a b) rest = do 
            b' <- expandSynonyms b
            fromAppT a (b' : rest)
        fromAppT other rest = return $ other : rest
        
        -- While we're at it, we'll discard polymorphism.
        stripForalls :: [Type] -> Q [Type]
        stripForalls ((ForallT _ _ t):rest) = do
            t' <- expandSynonyms t
            rest' <- stripForalls rest
            return $ t': rest'
        stripForalls (x:xs) = stripForalls xs >>= return.(x:)
        stripForalls [] = return []
         
        applySynonyms :: [Type] -> Q [Type]
        applySynonyms args@((ConT n):typs) = do
            info <- reify n
            case info of
                TyConI (TySynD _ args body) -> do
                    -- so when we apply our synonym and remove them from the 
                    -- list we might have a synonym left over!  So return the 
                    -- whole list and applySynonyms again.  Essentially the 
                    -- only way we can ever move forward through the list is if 
                    -- the head element, after expansion, is not a type synonym
                    -- constructor
                    body' <- expandSynonyms (substTy taken body)
                    syns <- applySynonyms $ body' : rest
                    -- runIO $ showAST syns
                    return syns
                    where
                        taken = zip args typs
                        rest = drop (length taken) typs
                _ -> skip1AndContinue args
        applySynonyms args = skip1AndContinue args
        
        skip1AndContinue (t:typs)= do
            typs' <- applySynonyms(typs)
            return $ t:typs'
        skip1AndContinue t = return t
        
        -- use mapping to replace all occurances of types.
        -- the ForallT has to exclude names that were used as polymorphic type 
        -- names, since they are unrelated to the types we're intending to
        -- substitute.
        -- Note that we still have ForallTs here because substTy is called on 
        -- the result of a reify
        substTy mapping (ForallT names cxt t)
            = ForallT names cxt (substTy mapping' t)
            where mapping' = filter (not . (`elem` names) . fst) mapping
        substTy mapping (VarT name)
            = fromMaybe (VarT name) (lookup name (map (first extractName) mapping))
        substTy mapping (AppT a b)
            = AppT (substTy mapping a) (substTy mapping b)
        substTy _ other
            = other

extractName (PlainTV n) = n
extractName (KindedTV n _) = n


toplevelConstructor (AppT a b) = toplevelConstructor a
toplevelConstructor (ConT n)   = Just n
toplevelConstructor _          = Nothing

repTypeName t
    = case toplevelConstructor t of
        Just t | t == ''ID      -> Just ptr
               | t == ''SEL     -> Just ptr
               | t == ''Ptr     -> Just ptr
               | t == ''CInt    -> Just int
               | t == ''CUInt   -> Just int
               | t == ''Int     -> Just hInt
               | t == ''Word    -> Just hInt
               | t == ''()      -> Just "void"
               | t == ''CChar   -> Just "char"
               | t == ''CUChar  -> Just "char"
               | t == ''CShort  -> Just "short"
               | t == ''CUShort -> Just "short"
               | t == ''CLLong  -> Just "llong"
               | t == ''CULLong -> Just "llong"
               | t == ''Float   -> Just "float"
               | t == ''Double  -> Just "double"
               | t == ''Bool    -> Just "char"
        _ -> Nothing
    where
        ptr  = "word"
        int  = "word"    -- ### NOT TRUE FOR 64 BITS
        hInt = "word"

-- for quoted type qt, this returns 
getCifTypeName qt
    = do
        --runIO (putStrLn "Input" >> ppQ qt >> putStrLn "expandSynonyms:")
        t <- expandSynonyms =<< qt
        --runIO (ppAST t)

        -- arrowsToList --
        -- converts a type of a->b->c->d-> IO e to an
        -- array of the from [a b c d e]
        let arrowsToList (AppT (AppT ArrowT a) b)
                = a : arrowsToList b
            arrowsToList (AppT (ConT c) b)
                | c == ''IO
                = [b]
        return $ fmap (concat . intersperse "_") $
                 mapM repTypeName $ arrowsToList t

-- given a list of types will transform them with getCifTypeName and return a 
-- list of declarations:
-- first there will be the declaration of cannedCIFTypeNames = [list of all 
-- type names made]
-- Then there will be a declaration, one for each in the above list created 
-- with makeCannedCIF below
makeCannedCIFs types
    = do
        mbCanned <- mapM (\t -> do {- Q -}
                                   mbName <- getCifTypeName t
                                   return $ (mbName >>= Just . makeCannedCIF t))
                         types               
        let (names, decls) = unzip $ catMaybes mbCanned

        decss <- sequence decls
        typeListDec <- [d| cannedCIFTypeNames = $(listE $ map stringE names) |]
        
        return $ typeListDec ++ concat decss

    where
        -- turns a type and a name into a tuple:
        -- name and the valD : cannedCIF_n = getCifForSelector (undefined :: t) 
        makeCannedCIF t n = (n, sequence [valD (varP $ cannedCIFName n) 
                                     (normalB [| getCifForSelector $(e) |])
                                     []] )
            where e = [| undefined |] `sigE` t
            
        cannedCIFName n = mkName $ "cannedCIF_" ++ n

staticCifForSelectorType master ns t
    = do
        mbName <- getCifTypeName t
        xt <- t
        case mbName of
            Just n | n `elem` ns
                -> varE $ ("cannedCIF_" ++ n) `fromSameModuleAs_v` master
            _ ->   [| getCifForSelector $( [| undefined |] `sigE` t) |]
