module HOC.Class where

import HOC.Base
import HOC.Arguments
import HOC.ID
import HOC.MessageTarget

import Foreign
import Foreign.C.String


data Class_ a
type Class a = ID (Class_ a)



unsafeGetClassObject :: String -> Class a


foreign import ccall unsafe "Class.h getClassByName"
	c_getClassByName :: CString -> IO (Ptr ObjCObject)
	
getClassByName name = withCString name c_getClassByName
	
{-# NOINLINE unsafeGetClassObject #-} -- called from generated code, save space
unsafeGetClassObject name = unsafePerformIO $
	getClassByName name >>= importImmortal




class (Object a, Object b) => ClassAndObject a b | a -> b, b -> a

instance ClassAndObject (Class a) (ID a)
