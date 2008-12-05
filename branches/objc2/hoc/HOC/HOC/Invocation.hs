{-# LANGUAGE ForeignFunctionInterface #-}
module HOC.Invocation where

import Foreign
import Foreign.C            ( CInt )
import Control.Monad        ( when )

import HOC.Base
import HOC.Arguments
import HOC.FFICallInterface

import HOC.Exception

foreign import ccall "Invocation.h callWithExceptions"
    c_callWithExceptions :: FFICif -> FunPtr a
                        -> Ptr b -> Ptr (Ptr ())
                        -> IO (Ptr ObjCObject) {- NSException -}

callWithException cif fun ret args = do
    exception <- c_callWithExceptions cif fun ret args
    when (exception /= nullPtr) $
        exceptionObjCToHaskell exception

withMarshalledArgument :: ObjCArgument a b => a -> (Ptr () -> IO c) -> IO c

withMarshalledArgument arg act =
    withExportedArgument arg (\exported -> with exported (act . castPtr))

callWithoutRetval :: FFICif -> FunPtr a
                  -> Ptr (Ptr ())
                  -> IO ()

callWithoutRetval cif fun args = callWithException cif fun nullPtr args


callWithRetval :: ObjCArgument b c
               => FFICif -> FunPtr a
               -> Ptr (Ptr ())
               -> IO b

callWithRetval cif fun args = do
    allocaRetval $ \retptr ->
        callWithException cif fun retptr args
        >> peekRetval retptr >>= importArgument


setMarshalledRetval :: ObjCArgument a b => Bool -> Ptr () -> a -> IO ()
setMarshalledRetval retained ptr val =
    (if retained then exportArgumentRetained else exportArgument) val
        >>= poke (castPtr ptr)

getMarshalledArgument :: ObjCArgument a b => Ptr (Ptr ()) -> Int -> IO a
getMarshalledArgument args idx = do
    p <- peekElemOff args idx
    arg <- peek (castPtr p)
    importArgument arg
    
    
foreign import ccall unsafe recordHOCEvent :: CInt -> Ptr (Ptr ()) -> IO ()

kHOCEnteredHaskell = 1 :: CInt
kHOCImportedArguments = 2 :: CInt
kHOCAboutToExportResult = 3 :: CInt
kHOCAboutToLeaveHaskell = 4 :: CInt


