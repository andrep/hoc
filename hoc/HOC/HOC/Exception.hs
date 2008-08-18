module HOC.Exception where

import Data.Typeable
import Foreign
import Foreign.C.String     ( CString, withCString )
import Prelude              hiding ( catch )
import Control.Exception    ( evaluate, throwIO, throwDyn, catchDyn, catch )

import HOC.Base
import HOC.Arguments
import HOC.ID

data WrappedNSException = WrappedNSException (ID ())
    deriving Typeable


foreign import ccall unsafe wrapHaskellException :: CString -> StablePtr a -> IO (Ptr ObjCObject)
foreign import ccall unsafe unwrapHaskellException :: Ptr ObjCObject -> IO (StablePtr a)

exceptionObjCToHaskell :: Ptr ObjCObject -> IO a

-- get the exception pointer figure out if it is a NSException
-- or a haskell exception and throw it.
exceptionObjCToHaskell exception = do
    sptr <- unwrapHaskellException exception
    if (castStablePtrToPtr sptr == nullPtr)
        then do
            exc <- importArgument exception
            evaluate $ throwDyn $ WrappedNSException exc
        else do
            exc <- deRefStablePtr sptr
            throwIO exc

exceptionHaskellToObjC :: IO a -> IO (Ptr ObjCObject)

exceptionHaskellToObjC action = 
    (action >> return nullPtr)
        `catchDyn`
    (\(WrappedNSException exc) -> exportArgument exc)
        `catch`
    (\exc -> withCString (show exc) $ \cstr -> newStablePtr exc >>= wrapHaskellException cstr)
