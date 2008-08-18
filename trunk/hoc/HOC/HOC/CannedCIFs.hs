module HOC.CannedCIFs where

import HOC.Base         ( SEL )
import HOC.Arguments    ( getCifForSelector )
import HOC.ID           ( ID )
import HOC.TH           ( fromSameModuleAs_v )

import Data.List        ( intersperse )
import Data.Maybe       ( catMaybes )
import Data.Word        ( Word )
import Foreign          ( Ptr )
import Foreign.C
import Language.Haskell.TH

-- expandSynonyms (VarT Name) = (VarT Name)
-- expandSynonyms AppT (ConsT Name') (VarT Name) = typ >>= expandSynonyms1 
-- (ConstT Name') (VarT Name)
-- which goes back too AppT (ConsT Name') (VarT Name)
--
-- Transforms the type in the following way:
-- This effectively discards all polymorphism and type synonyms.
-- * Simple types (VarT, TupleT, ArrowT, ListT) are returned as is.
-- * Type applications are unpacked, first the latter half is expanded,
-- then the former have is expanded.
-- * Polymorphism is discarded
-- * If a constructor is encountered, the type of the constuctor is inspected.  
-- If that constructor is constructing a type synonym then the type synonym is 
-- removed by consuming a number of soon-to-be applyed types (stored in 
-- "pending") equal to the number of arguments in the type synonym, discarding 
-- the type synonym construct, and substituting the type of the arguments to 
-- the body of the type synonym (thus effectively removing all type synonyms)
-- 
expandSynonyms typ
    = typ >>= flip expandSynonyms1 []
    where
        -- unwrap the AppT, expand b, push b' onto the pending list
        expandSynonyms1 (AppT a b) pending
            = do
                b' <- expandSynonyms1 b []
                expandSynonyms1 a (b' : pending)
        -- grab the types, expand them, then fold up the expanded types
        -- and everything that is pending (thus removing the ForallT)
        expandSynonyms1 (ForallT vars ctx t) pending
            = do
                t' <- expandSynonyms1 t []
                return $ foldl AppT t' pending
        -- n is a type synonym, removed it by substuting pending arguments
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
        -- this is the simple type termination condition.
        -- return Q (AppT (AppT A B) C)
        -- which is to say ((A B) C)
        expandSynonyms1 other pending    -- VarT, TupleT, ArrowT, ListT
            = return $ foldl AppT other pending
        
        -- use mapping to replace all occurances of types.
        -- the ForallT has to exclude names that were used as polymorphic type 
        -- names, since they are unrelated to the types we're intended to 
        -- substitute.
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
        t <- expandSynonyms qt
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
