module HOC.TH(
        module Language.Haskell.TH,
        mkNameG_v,
        mkNameG_tc,
        mkNameG_d,
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance Functor Q where
    fmap f q = q >>= return . f

