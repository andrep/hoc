module HOC.TH(
        module Language.Haskell.TH,
        mkNameG_v,
        mkNameG_tc,
        mkNameG_d,
        whereQ
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance Functor Q where
    fmap f q = q >>= return . f

whereQ :: ([Q Dec] -> Q a) -> Q [Dec] -> Q a
header `whereQ` declsQ = do
    decls <- declsQ
    header (map return decls)

