{-# OPTIONS -fglasgow-exts #-}
module CTypeToHaskell(TypeEnvironment(..),
                      TypeNameKind(..),
                      isClassType,
                      isPlainType,
                      typeDefinedIn,
                      lookupTypeEnv,
                      pprSelectorType,
                      HSelectorType(..),
                      HTypeTerm(..),
                      SelectorKind(..),
                      getSelectorType,
                      getVariableType,
                      pprVariableType,
                      HType(..),
                      mentionedTypes,
                      varMentionedTypes) where

import SyntaxTree
import Headers(ModuleName)

import HOC.NameCaseChange

import Control.Monad(when)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import Text.PrettyPrint

import Debug.Trace
import Data.Generics(Typeable,Data)

data TypeNameKind = ClassTypeName | PlainTypeName
    deriving (Show)

newtype TypeEnvironment = TypeEnvironment (Map.Map String (TypeNameKind, ModuleName))
    -- (Set String) -- known classes
    -- (Set String) -- other known types

isClassType (TypeEnvironment env) name =
    case Map.lookup (nameToUppercase name) env of
        Just (ClassTypeName, _) -> True
        _                       -> False
isPlainType (TypeEnvironment env) name =
    case Map.lookup (nameToUppercase name) env of
        Just (PlainTypeName, _) -> True
        _                       -> False
        
typeDefinedIn (TypeEnvironment env) name =
    case Map.lookup (nameToUppercase name) env of
        Just (_, loc) -> loc

lookupTypeEnv (TypeEnvironment env) name = Map.lookup name env

data HTypeTerm = Con String | HTypeTerm :$ HTypeTerm | Var String
    deriving(Read,Eq,Ord,Show,Typeable,Data)
    
data HType = HType (Maybe (String, [String])) [String] HTypeTerm
    -- Maybe (tyvar, context), mentioned, terms
    deriving(Read,Eq,Ord,Show,Typeable,Data)

data HSelectorType = HSelectorType [String] [(String,String)] [String] [HTypeTerm]
    deriving(Read,Eq,Ord,Show,Typeable,Data)

cTypeToHaskell :: TypeEnvironment -> Bool -> String -> CType -> Maybe HType
cTypeToHaskell env retval tyvar (CTIDType protocols) = 
    -- (if protocols /= [] then trace (show (retval,protocols)) else id) $

    return $ HType (if retval then Nothing else Just (tyvar,[]))
                 [] (Con "ID" :$ (if retval then Con "()" else Var tyvar))

cTypeToHaskell env retval tyvar (CTPointer (CTSimple cls))
    | isClassType env cls =
    return $ HType (if retval then Nothing else Just (tyvar,[]))
                 [cls] (Con (nameToUppercase cls) :$
                        (if retval then Con "()" else Var tyvar))
 

-- cTypeToHaskell classes retval tyvar (CTBuiltin signedness len "int") =
--    case signedness of 
cTypeToHaskell env retval tyvar bi@(CTBuiltin _ _ _) =
    do
        typ <- builtinTypeToHaskell bi
        return $ HType Nothing [] (Con typ)

cTypeToHaskell env retval tyvar (CTSimple "Class") =
    return $ HType (if retval then Nothing else Just (tyvar,[]))
                 [] (Con "Class" :$
                        (if retval then Con "()" else Var tyvar))

cTypeToHaskell env retval tyvar (CTSimple name)
    | name /= "" && isPlainType env name =
        return $ HType Nothing [name]
                       (Con $ nameToUppercase name)
    | otherwise = case simpleTypeToHaskell name of
                    Just typ -> return $ HType Nothing [] (Con typ)
                    Nothing -> -- trace ("type not found: " ++ show name) 
                                Nothing

cTypeToHaskell env retval tyvar (CTPointer pointed)
    = do
        HType context mentioned ty
            <- cTypeToHaskell env True tyvar pointed
        return $ HType context mentioned (Con "Ptr" :$ ty)

cTypeToHaskell env retval tyvar (CTEnum name _) 
    | name /= "" && isPlainType env name = return $ HType Nothing [name]
                                                          (Con $ nameToUppercase name)
    | otherwise = Nothing

cTypeToHaskell env retval tyvar _ = Nothing

simpleTypeToHaskell "void" = return "()"
simpleTypeToHaskell "BOOL" = return "Bool"
simpleTypeToHaskell "SEL" = return "SEL"
simpleTypeToHaskell _ = Nothing

builtinTypeToHaskell (CTBuiltin Nothing Nothing "float") = return "Float"
builtinTypeToHaskell (CTBuiltin Nothing Nothing "double") = return "Double"
builtinTypeToHaskell (CTBuiltin signedness Nothing "int") =
    case signedness of
        Just False -> return "CUInt"
        _ -> return "CInt"
builtinTypeToHaskell (CTBuiltin signedness (Just Short) "int") =
    case signedness of
        Just False -> return "CUShort"
        _ -> return "CShort"
builtinTypeToHaskell (CTBuiltin signedness (Just Long) "int") =
    case signedness of
        Just False -> return "CULong"
        _ -> return "CLong"
builtinTypeToHaskell (CTBuiltin signedness (Just LongLong) "int") =
    case signedness of
        Just False -> return "CULLong"
        _ -> return "CLLong"
builtinTypeToHaskell (CTBuiltin signedness Nothing "char") =
    case signedness of
        Just False -> return "CUChar"
        Just True -> return "CSChar"
        Nothing -> return "CChar"

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

getSelectorType' env sel ret = do
    when (selVarArg sel) Nothing
    argTypes <- sequence $
                zipWith (cTypeToHaskell env False)
                        [ "t" ++ show i | i <- [1..] ] (selArgTypes sel)
    return $ liftContexts (argTypes ++ [ret])

data SelectorKind = PlainSelector 
                  | CovariantSelector
                  | CovariantInstanceSelector
                  | AllocSelector
                  | InitSelector
    deriving (Read,Eq,Ord,Show,Typeable,Data)
getSelectorType :: SelectorKind -> TypeEnvironment -> Selector -> Maybe HSelectorType

getSelectorType PlainSelector env sel = do
    HType retTypeTyCtx retTypeMentioned retType <-
        (cTypeToHaskell env True) "t" $ selRetType sel
    let ioRetType = HType retTypeTyCtx retTypeMentioned (Con "IO" :$ retType)
    getSelectorType' env sel ioRetType

getSelectorType kind env sel =
    getSelectorType' env sel $
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

mentionedTypes (HSelectorType tyvars context mentioned types) = mentioned


getVariableType :: TypeEnvironment -> CType -> Maybe HType
pprVariableType :: HType -> Doc

getVariableType env t = cTypeToHaskell env True (error "### getVariableType") t

pprVariableType (HType _ _ tt) = pprHTypeTerm False tt

varMentionedTypes (HType _ mentioned _) = mentioned
