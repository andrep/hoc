{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, 
             ScopedTypeVariables, CPP #-}
module HOC.Exception where

import Data.Typeable
import Foreign
import Foreign.C.String     ( CString, withCString )
import Prelude              hiding ( catch )
import Control.Exception

import HOC.Base
import HOC.Arguments
import HOC.ID

data WrappedNSException = WrappedNSException (ID ())
    deriving Typeable

exceptionObjCToHaskell :: Ptr ObjCObject -> IO a

#ifdef BASE4

foreign import ccall unsafe wrapHaskellException
    :: CString -> StablePtr SomeException -> IO (Ptr ObjCObject)
foreign import ccall unsafe unwrapHaskellException
    :: Ptr ObjCObject -> IO (StablePtr SomeException)

-- get the exception pointer figure out if it is a NSException
-- or a haskell exception and throw it.
exceptionObjCToHaskell exception = do
    sptr <- unwrapHaskellException exception
    if (castStablePtrToPtr sptr == nullPtr)
        then do
            exc <- importArgument exception
            throwIO $ WrappedNSException exc
        else do
            exc <- deRefStablePtr sptr
            throwIO exc

exceptionHaskellToObjC :: IO a -> IO (Ptr ObjCObject)

exceptionHaskellToObjC action = 
    (action >> return nullPtr)
        `catches` [
            Handler $ \(WrappedNSException exc) -> exportArgument exc,
            Handler $ \(exc :: SomeException) -> withCString (show exc) $
                \cstr -> newStablePtr exc >>= wrapHaskellException cstr
        ]
        
instance Exception WrappedNSException where
    toException = SomeException
    fromException (SomeException ex) = cast ex
        
instance Show WrappedNSException where
    show (WrappedNSException ex) = "<<NSException>>"
        
catchWrappedNSException :: IO a -> (WrappedNSException -> IO a) -> IO a
catchWrappedNSException = catch

#else

foreign import ccall unsafe wrapHaskellException
    :: CString -> StablePtr Exception -> IO (Ptr ObjCObject)
foreign import ccall unsafe unwrapHaskellException
    :: Ptr ObjCObject -> IO (StablePtr Exception)

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

catchWrappedNSException :: IO a -> (WrappedNSException -> IO a) -> IO a
catchWrappedNSException = catchDyn

#endif
