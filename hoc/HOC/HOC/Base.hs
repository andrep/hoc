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

{-
newtype ID a = ID (ForeignPtr ObjCObject)

{- moved to Arguments.hs
class Object a where
	toID :: a -> ID ()
	
instance Object (ID a) where
	toID (ID a) = ID a
-}

castObject (ID a) = ID a

instance Eq (ID a) where
    (ID a) == (ID b) = a == b
-}

--



thModulePrefix mod id = "HOC." ++ mod ++ ":" ++ id
