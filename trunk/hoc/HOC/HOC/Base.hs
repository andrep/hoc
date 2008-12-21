{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
module HOC.Base where

import Foreign
import Foreign.C.String
import Control.Monad(when)
import Control.Exception(bracket)

-- The SEL Type

newtype SEL = SEL (Ptr ())
    deriving(Storable)

foreign import ccall unsafe "Selector.h getSelectorForName"
    c_getSelectorForName :: CString -> IO SEL  

getSelectorForName :: String -> SEL
getSelectorForName str = unsafePerformIO $
    withCString str (c_getSelectorForName)

--

foreign import ccall "stdlib.h &free"
    freePtr :: FunPtr (Ptr a -> IO ())

--

foreign import ccall "MemoryManagement.h retainObject"
    retainObject :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h releaseObject"
    releaseObject :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h deallocObject"
    deallocObject :: Ptr ObjCObject -> IO ()

foreign import ccall "MemoryManagement.h autoreleaseObject"
    autoreleaseObject :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h newAutoreleasePool"
    newAutoreleasePool :: IO (Ptr ObjCObject)

withAutoreleasePool action = bracket newAutoreleasePool releaseObject (const action)

--

data ObjCObject
