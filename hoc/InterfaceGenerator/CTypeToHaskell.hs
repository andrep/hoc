module CTypeToHaskell(pprSelectorType,
                      HSelectorType,
                      SelectorKind(..),
                      getSelectorType,
                      mentionedClasses) where

import Control.Monad(when)
import Data.Set
import Data.Maybe(mapMaybe)
import Text.PrettyPrint

import Debug.Trace

import SyntaxTree

data HTypeTerm = Con String | HTypeTerm :$ HTypeTerm | Var String
    deriving(Eq,Ord)
    
data HType = HType (Maybe (String, [String])) [String] HTypeTerm
    -- Maybe (tyvar, context), classes, terms

data HSelectorType = HSelectorType [String] [(String,String)] [String] [HTypeTerm]
    deriving(Eq,Ord)

cTypeToHaskell :: Set String -> Bool -> String -> CType -> Maybe HType
cTypeToHaskell classes retval tyvar (CTIDType protocols) = 
    -- (if protocols /= [] then trace (show (retval,protocols)) else id) $

    Just $ HType (if retval then Nothing else Just (tyvar,[]))
                 [] (Con "ID" :$ (if retval then Con "()" else Var tyvar))

cTypeToHaskell classes retval tyvar (CTPointer (CTSimple cls)) | cls `elementOf` classes =
    Just $ HType (if retval then Nothing else Just (tyvar,[]))
                 [cls] (Con cls :$ (if retval then Con "()" else Var tyvar))
 

-- cTypeToHaskell classes retval tyvar (CTBuiltin signedness len "int") =
--    case signedness of 
cTypeToHaskell classes retval tyvar bi@(CTBuiltin _ _ _) =
    do
        typ <- builtinTypeToHaskell bi
        return $ HType Nothing [] (Con typ)

cTypeToHaskell classes retval tyvar (CTSimple simple) =
    do
        typ <- simpleTypeToHaskell simple
        return $ HType Nothing [] (Con typ)

cTypeToHaskell classes retval tyvar (CTPointer pointed) =
        case pointed of
            CTSimple _ -> pointerToHaskell
            CTBuiltin _ _ _ -> pointerToHaskell
            _ -> Nothing -- we don't want to bother with things like "id *" right now
    where
        pointerToHaskell =
            do
                HType context mentioned ty
                    <- cTypeToHaskell classes retval tyvar pointed
                return $ HType context mentioned (Con "Ptr" :$ ty)

cTypeToHaskell classes retval tyvar _ = Nothing
{-
cTypeToHaskell classes retval tyvar (CTSimple str) = Nothing
cTypeToHaskell classes retval tyvar (CTPointer x) = Nothing
cTypeToHaskell classes retval tyvar (CTUnknown x) = Nothing
cTypeToHaskell classes retval tyvar (CTEnum x) = Nothing
cTypeToHaskell classes retval tyvar (CTStruct x) = Nothing
cTypeToHaskell classes retval tyvar (CTUnion x) = Nothing
-}



simpleTypeToHaskell "void" = Just "()"
simpleTypeToHaskell "BOOL" = Just "Bool"
simpleTypeToHaskell "SEL" = Just "SEL"
simpleTypeToHaskell other = {-trace ("unknown simple type " ++ show other)-} Nothing

{-builtinTypeToHaskell (CTBuiltin Nothing Nothing "int") =
    trace (
-}
builtinTypeToHaskell (CTBuiltin Nothing Nothing "float") = Just "Float"
builtinTypeToHaskell (CTBuiltin Nothing Nothing "double") = Just "Double"
builtinTypeToHaskell (CTBuiltin signedness Nothing "int") =
    case signedness of
        Just False -> Just "CUInt"
        _ -> Just "Int"
builtinTypeToHaskell (CTBuiltin signedness (Just Short) "int") =
    case signedness of
        Just False -> Just "CUShort"
        _ -> Just "CShort"
builtinTypeToHaskell (CTBuiltin signedness (Just Long) "int") =
    case signedness of
        Just False -> Just "CULong"
        _ -> Just "CLong"
builtinTypeToHaskell (CTBuiltin signedness (Just LongLong) "int") =
    case signedness of
        Just False -> Just "CULLong"
        _ -> Just "CLLong"
builtinTypeToHaskell (CTBuiltin signedness Nothing "char") =
    case signedness of
        Just False -> Just "CUChar"
        Just True -> Just "CSChar"
        Nothing -> Just "CChar"

builtinTypeToHaskell bi = trace (show bi) Nothing

liftContexts :: [HType] -> HSelectorType

liftContexts types = HSelectorType
        (mapMaybe (\(HType tyCtx _ _) -> fmap fst tyCtx) types)
        (concatMap (\(HType tyCtx _ _) -> 
                case tyCtx of
                    Just (tyvar, ctx) -> map (flip (,) tyvar) ctx
                    Nothing -> []
            ) types)
        (concatMap (\(HType _ classes _) -> classes) types)
        (map (\(HType _ _ typeTerm) -> typeTerm) types)

pprContext [] = empty
pprContext xs = error "pprContext unimplemented"

pprForall [] = empty
pprForall xs = text "forall" <+> hsep (map text xs) <+> text "."

pprHTypeTerm _ (Con s) = text s
pprHTypeTerm _ (Var tv) = text tv
pprHTypeTerm True t@(a :$ b) = parens (pprHTypeTerm False t)
pprHTypeTerm False (a :$ b) = pprHTypeTerm True a <+> pprHTypeTerm True b


pprSelectorType :: HSelectorType -> Doc

getSelectorType' classes sel ret = do
    when (selVarArg sel) Nothing
    argTypes <- sequence $
                zipWith (cTypeToHaskell classes False)
                        [ "t" ++ show i | i <- [1..] ] (selArgTypes sel)
    return $ liftContexts (argTypes ++ [ret])

data SelectorKind = PlainSelector 
                  | CovariantSelector
                  | CovariantInstanceSelector
                  | AllocSelector
                  | InitSelector

getSelectorType :: SelectorKind -> Set String -> Selector -> Maybe HSelectorType

getSelectorType PlainSelector classes sel = do
    HType retTypeTyCtx retTypeMentioned retType <-
        (cTypeToHaskell classes True) "t" $ selRetType sel
    let ioRetType = HType retTypeTyCtx retTypeMentioned (Con "IO" :$ retType)
    getSelectorType' classes sel ioRetType

getSelectorType kind classes sel =
    getSelectorType' classes sel $
        HType Nothing [] (Con "IO" :$
            (case kind of
                CovariantSelector -> Con "Covariant"
                CovariantInstanceSelector -> Con "CovariantInstance"
                AllocSelector -> Con "Allocated"
                InitSelector -> Con "Inited"
            ))

pprSelectorType (HSelectorType tyvars context mentioned types) =
    pprForall tyvars <+> pprContext context <+>
    (hsep $ punctuate (text " ->") $ map (pprHTypeTerm False) types)

mentionedClasses (HSelectorType tyvars context mentioned types) = mentioned

