module HOC.TH(
        module Language.Haskell.TH,
        mkNameG_v,
        mkNameG_tc,
        mkNameG_d,
        whereQ,
        fromSameModuleAs_tc,
        fromSameModuleAs_v
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- instance Functor Q where
--    fmap f q = q >>= return . f

whereQ :: ([Q Dec] -> Q a) -> Q [Dec] -> Q a
header `whereQ` declsQ = do
    decls <- declsQ
    header (map return decls)

{-
fromSameModuleAs_tc :: String -> Name -> Name
s `fromSameModuleAs_tc` n
    = case nameModule n of
        Nothing -> mkName s
        Just m -> mkNameG_tc m s
        
fromSameModuleAs_v :: String -> Name -> Name
s `fromSameModuleAs_v` n
    = case nameModule n of
        Nothing -> mkName s
        Just m -> mkNameG_v m s
-}

fromSameModuleAs_tc :: String -> Name -> Name
fromSameModuleAs_tc = fromSameModule TcClsName
fromSameModuleAs_v :: String -> Name -> Name
fromSameModuleAs_v = fromSameModule VarName

fromSameModule :: NameSpace -> String -> Name -> Name
fromSameModule ns s n
  = Name (mkOccName s) $
  	case n of
  		Name _ (NameG _ pkg mod) -> NameG ns pkg mod
  		Name _ other -> other
