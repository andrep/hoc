module Enums(
        EnumType,
        extractEnums,
        pprEnumType,
        enumName
    ) where

import Headers(HeaderInfo(..), ModuleName)
import SyntaxTree
import BindingScript    ( BindingScript(bsHiddenEnums) )

import HOC.NameCaseChange

import Data.Char        ( toUpper )
import Data.Maybe       ( mapMaybe )
import qualified Data.Map as Map   ( Map, fromList )
import Data.Set         ( Set, mkSet, elementOf )
import Text.PrettyPrint.HughesPJ
import Debug.Trace

extractEnums :: BindingScript -> [HeaderInfo] -> ([(String, ModuleName)], Map.Map ModuleName [EnumType])

data EnumType = EnumType (Maybe String) [(String, Integer)] deriving(Show)

enumName (EnumType mbName _) = mbName

extractEnums bs headers =
        ( [ (name, mod) | (mod, types) <- enums, Just name <- map enumName types ]
        , Map.fromList enums
        )
    where enums = [ (moduleName, mapMaybe (filterEnumType bs . extractEnumType) decls)
                  | HeaderInfo moduleName _ decls <- headers ]


filterEnumType :: BindingScript -> Maybe EnumType -> Maybe EnumType
filterEnumType bs (Just (EnumType (Just name) _)) | name `elementOf` bsHiddenEnums bs = Nothing
filterEnumType _ mbTy = mbTy
                  
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

pprEnumType (EnumType name constants) =
	char '$' <> parens (
			declare
			<+> brackets (
				hcat $ punctuate comma $ map pprAssoc constants
			)
		)
	where
		declare = case name of
			Just cname -> text "declareCEnum" <+> doubleQuotes (text cname)
			Nothing    -> text "declareAnonymousCEnum"
		pprAssoc (n, v)
			= parens (doubleQuotes (text n) <> comma <+> integer v)
