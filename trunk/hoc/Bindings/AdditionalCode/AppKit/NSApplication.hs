--X nsApplicationMain_

import Foreign            ( withMany, withArray0, Ptr, nullPtr )
import Foreign.C          ( withCString, CInt, CString )
import System.Environment ( getProgName, getArgs )
import Prelude

-- CUT HERE

foreign import ccall "NSApplicationMain" c_nsApplicationMain
	:: CInt -> Ptr CString -> IO CInt

nsApplicationMain2 prog args =
	withMany withCString (prog : args) $ \argvPtrs ->
	withArray0 nullPtr argvPtrs $ \argvBuf ->
	c_nsApplicationMain (1 + (fromIntegral $ length args)) argvBuf

nsApplicationMain_ = do
    prog <- getProgName
    args <- getArgs
    nsApplicationMain2 prog args
