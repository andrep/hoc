module Enums(
        EnumType,
        extractEnums,
        pprEnumType,
        enumName
    ) where

import Headers(HeaderInfo(..), ModuleName)
import SyntaxTree
import NameCaseChange

import Data.Char        ( toUpper )
import Data.Maybe       ( mapMaybe )
import Data.FiniteMap   ( FiniteMap, listToFM )
import Data.Set         ( Set, mkSet )
import Text.PrettyPrint.HughesPJ

extractEnums :: [HeaderInfo] -> ([(String, ModuleName)], FiniteMap ModuleName [EnumType])

data EnumType = EnumType (Maybe String) [(String, Integer)]

enumName (EnumType mbName _) = mbName

extractEnums headers =
        ( [ (name, mod) | (mod, types) <- enums, Just name <- map enumName types ]
        , listToFM enums
        )
    where enums = [ (moduleName, mapMaybe extractEnumType decls)
                  | HeaderInfo moduleName _ decls <- headers ]


extractEnumType :: Declaration -> Maybe EnumType

extractEnumType (CTypeDecl t) = handleCType t
extractEnumType (Typedef t name) =
    handleCType t
    >>= \(EnumType _ constants) -> return $ EnumType (Just name) constants
extractEnumType _ = Nothing

handleCType :: CType -> Maybe EnumType

handleCType (CTEnum tag []) = Nothing   -- enum decl without body
handleCType (CTEnum tag constants)
    | all (simpleEnough . snd) constants = Just $ EnumType mbTag (f 0 constants)
    where
        simpleEnough (TooComplicatedValue _) = False
        simpleEnough _                       = True
        f _ [] = []
        f nextVal ((name, value) : xs)
            = case value of
                NextValue        -> (name, nextVal) : f (nextVal + 1) xs
                GivenValue given -> (name, given) : f (given + 1) xs
        mbTag | tag == "" = Nothing
              | otherwise = Just tag
handleCType _ = Nothing

pprEnumType (EnumType (Just cname) constants) =
        dataDeclaration $+$ instanceDeclaration
    where
        name = nameToUppercase cname
        
        dataDeclaration = text "data" <+> text name
                        <+> conDecls
        conDecls = vcat $ zipWith (<+>) (equals : repeat (char '|'))
                                        (map text constructors)


        constructors = map (nameToUppercase . fst) constants
        values = map snd constants
        
        
        instanceDeclaration =
            hang (text "instance ObjCArgument"
                  <+> text name <+> text "CInt where")
                 4
                 (exports $$ imports $$ typestr)
        
        exports = vcat [ text "exportArgument" <+> text con <+> text "= return" <+> hInteger val
                       | (con,val) <- zip constructors values ]
        imports = vcat [ text "importArgument" <+> hInteger val <+> text "= return" <+> text con
                       | (con,val) <- zip constructors values ]
        
        hInteger i | i < 0     = parens (integer i)
                   | otherwise = integer i
                
        typestr = text "objCTypeString _ = \"i\""
        
pprEnumType (EnumType Nothing constants) =
           text "{- ### anonymous enum!"
        $$ nest 4 (pprEnumType $ EnumType (Just "Anon") constants)
        $$ text "### -}"
