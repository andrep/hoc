{-# LANGUAGE PatternGuards #-}
module HackEnumNames where

import SyntaxTree
import Headers

hackEnumNames (HeaderInfo name imports decls)
    = HeaderInfo name imports (hackEnums1 Just id decls)
    where
        hackEnums1 :: (a -> Maybe Declaration) -> (Declaration -> a) -> [a] -> [a]
        hackEnums1 unwrap wrap (x : y : xs)
            | Just (CTypeDecl (CTEnum name1 vals)) <- unwrap x,
              Just (Typedef (CTSimple baseType) name2) <- unwrap y,
              null name1 || name1 == name2 || name1 == '_' : name2,
              baseType == "NSInteger" || baseType == "NSUInteger"
            = wrap (Typedef (CTEnum name1 vals) name2)
                : hackEnums1 unwrap wrap xs
        hackEnums1 unwrap wrap (x : xs)
            | Just (SelectorList header items) <- unwrap x
            = wrap (SelectorList header (hackEnums1 decl LocalDecl items))
                : hackEnums1 unwrap wrap xs
            | otherwise
            = x : hackEnums1 unwrap wrap xs
            where decl (LocalDecl d) = Just d
                  decl other = Nothing
        hackEnums1 unwrap wrap [] = []
        
