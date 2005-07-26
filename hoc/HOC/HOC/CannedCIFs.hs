module HOC.CannedCIFs where

import HOC.Base         ( SEL )
import HOC.Arguments    ( getCifForSelector )
import HOC.ID           ( ID )
import HOC.TH           ( mkNameG_v )

import Data.List        ( intersperse )
import Data.Maybe       ( catMaybes )
import Data.Word        ( Word )
import Debug.Trace
import Foreign          ( Ptr )
import Foreign.C
import Language.Haskell.TH

import System.IO

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
                runIO $ (print n >> hFlush stdout)
                info <- reify n
                case info of
                    TyConI (TySynD _ args body) ->
                            expandSynonyms1 (substTy taken body) rest
                        where
                            taken = zip args pending
                            rest = drop (length taken) pending
                    _ -> return $ foldl AppT (ConT n) pending
        expandSynonyms1 other pending    -- VarT, TupleT, ArrowT, ListT
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
        
getCifTypeName qt
    = do
        t <- expandSynonyms qt
        let arrowsToList (AppT (AppT ArrowT a) b)
                = a : arrowsToList b
            arrowsToList (AppT (ConT c) b)
                | c == ''IO
                = [b]
        return $ fmap (concat . intersperse "_") $
                 mapM repTypeName $ arrowsToList t

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
        makeCannedCIF t n = (n, sequence [valD (varP $ cannedCIFName n) 
                                     (normalB [| getCifForSelector $(e) |])
                                     []] )
            where e = [| undefined |] `sigE` t
            
        cannedCIFName n = mkName $ "cannedCIF_" ++ n

staticCifForSelectorType mod ns t
    = do
        mbName <- getCifTypeName t
        xt <- t
        case mbName of
            Just n | n `elem` ns
                -> trace ("USING: " ++ n) $ varE $ mkNameG_v mod $ "cannedCIF_" ++ n
            _ ->   trace ("NOT USING: " ++ show mbName ++ " " ++ show xt) $
                    [| getCifForSelector $( [| undefined |] `sigE` t) |]
