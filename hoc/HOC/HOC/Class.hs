{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances #-}
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

class ClassAndObject a b => StaticClassAndObject a b
    where 
        -- _staticClassForObject must not touch its parameter:
        -- its value should only depend on the type of the parameter.
        _staticClassForObject :: b -> a

-- make an export-safe version; don't want people making new
-- implementations, but they should be allowed to use the info.
staticClassForObject :: StaticClassAndObject a b => b -> a
staticClassForObject = _staticClassForObject