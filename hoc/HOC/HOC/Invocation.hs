module HOC.Invocation where

import Foreign
import Control.Monad ( when )
import Control.Exception ( bracket )

import HOC.Base
import HOC.Arguments
import HOC.FFICallInterface

foreign import ccall "Invocation.h callWithExceptions"
    c_callWithExceptions :: FFICif -> FunPtr a
                        -> Ptr b -> Ptr (Ptr ())
                        -> IO (Ptr ObjCObject) {- NSException -}

callWithException cif fun ret args = do
    exception <- c_callWithExceptions cif fun ret args
    when (exception /= nullPtr) $
        error "## exception marshalling not yet implemented ###"

withMarshalledArgument :: ObjCArgument a b => a -> (Ptr () -> IO c) -> IO c

withMarshalledArgument arg act =
    withExportedArgument arg (\exported -> withObject exported (act . castPtr))

callWithoutRetval :: FFICif -> FunPtr a
                  -> Ptr (Ptr ())
                  -> IO ()

callWithoutRetval cif fun args = callWithException cif fun nullPtr args


callWithRetval :: ObjCArgument b c
               => FFICif -> FunPtr a
               -> Ptr (Ptr ())
               -> IO b

callWithRetval cif fun args = do
    alloca $ \retptr ->
        callWithException cif fun retptr args
        >> peek retptr >>= importArgument


setMarshalledRetval :: ObjCArgument a b => Ptr () -> a -> IO ()
setMarshalledRetval ptr val =
    exportArgument val >>= poke (castPtr ptr)

getMarshalledArgument :: ObjCArgument a b => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    arg <- peek (castPtr p)
    importArgument arg

exceptionHaskellToObjC action = 
    action >> return nullPtr {- ### `catch` return some nsexception -}
 