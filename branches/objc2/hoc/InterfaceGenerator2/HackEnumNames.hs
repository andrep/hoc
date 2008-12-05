{-# LANGUAGE PatternGuards #-}
module HackEnumNames where

import SyntaxTree
import Headers

hackEnumNames :: HeaderInfo -> HeaderInfo

hackEnumNames (HeaderInfo headerName imports decls)
    = HeaderInfo headerName imports (hackEnums1 Just id decls)
    where
        hackEnums1 :: (a -> Maybe DeclarationAndPos) -> (DeclarationAndPos -> a) -> [a] -> [a]
        hackEnums1 unwrap wrap (x : y : xs)
            | Just (pos, CTypeDecl (CTEnum name1 vals)) <- unwrap x,
              Just (_, Typedef baseType name2) <- unwrap y,
              null name1 && acceptableEnumBaseType name2 baseType || acceptableEnumTag name1 name2
            = wrap (pos, Typedef (CTEnum name1 vals) name2)
                : hackEnums1 unwrap wrap xs
        hackEnums1 unwrap wrap (x : xs)
            | Just (pos, SelectorList header items) <- unwrap x
            = wrap (pos, SelectorList header (hackEnums1 undecl decl items))
                : hackEnums1 unwrap wrap xs
            | otherwise
            = x : hackEnums1 unwrap wrap xs
            where undecl (pos, LocalDecl d) = Just (pos, d)
                  undecl other = Nothing
                  decl (pos, d) = (pos, LocalDecl d)
        hackEnums1 unwrap wrap [] = []
        
        acceptableEnumBaseType name2 (CTSimple name)
            | name == "NSInteger" || name == "NSUInteger" = True
        acceptableEnumBaseType _ _ = False

        acceptableEnumTag name1 name2
            = dropWhile (== '_') name1 == name2
