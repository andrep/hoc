module HOC.Dyld(
        lookupSymbol,
        lookupSymbol#
    ) where

import GHC.Exts(Ptr(..), Addr#)
import Foreign
import Foreign.C.String

    -- Up to GHC 6.4.0, there was a bug where rtldDefault was marshalled
    -- incorrectly for Mac OS X, so we do it by hand.

foreign import ccall "dlsym" c_dlsym :: Ptr () -> CString -> IO (FunPtr a)

rtldDefault = nullPtr `plusPtr` (-2)

lookupSymbol :: String -> IO (FunPtr a)
lookupSymbol# :: Addr# -> IO (FunPtr a)

lookupSymbol name = withCString name (c_dlsym rtldDefault)
lookupSymbol# name# = c_dlsym rtldDefault (Ptr name#)
