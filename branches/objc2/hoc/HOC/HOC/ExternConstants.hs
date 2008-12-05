{-# LANGUAGE TemplateHaskell, MagicHash #-}
module HOC.ExternConstants(declareExternConst) where

import HOC.TH
import HOC.Arguments
import HOC.NameCaseChange
import HOC.Dyld

import Foreign

declareExternConst :: String -> TypeQ -> Q [Dec]

declareExternConst name typ
    = sequence [
        sigD n typ,
        valD (varP n) (normalB expr) []
    ]
    where
        n = mkName $ nameToLowercase name
        expr = [| getGlobalVar $(stringE name) |]

getGlobalVar name = unsafePerformIO $
    lookupSymbol name
    >>= peek . castFunPtrToPtr
    >>= importArgument

getGlobalVar# name# = unsafePerformIO $
    lookupSymbol# name#
    >>= peek . castFunPtrToPtr
    >>= importArgument