{-# LANGUAGE MagicHash #-}
module HOC.Dyld(
        lookupSymbol,
        lookupSymbol#
    ) where

import GHC.Exts(Ptr(..), Addr#)
import Foreign
import Foreign.C.String
import System.Posix.DynamicLinker

lookupSymbol :: String -> IO (FunPtr a)
lookupSymbol# :: Addr# -> IO (FunPtr a)

lookupSymbol = dlsym Default
lookupSymbol# name# = c_dlsym (packDL Default) (Ptr name#)

