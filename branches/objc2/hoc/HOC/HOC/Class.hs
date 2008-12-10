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
type MetaClass a = Class (Class_ a)


unsafeGetClassObject :: String -> Class a

foreign import ccall unsafe "Class.h getClassByName"
    c_getClassByName :: CString -> IO (Ptr ObjCObject)

getClassByName name = withCString name c_getClassByName

{-# NOINLINE unsafeGetClassObject #-} -- called from generated code, save space
unsafeGetClassObject name = unsafePerformIO $
    getClassByName name >>= importImmortal


unsafeGetMetaclassForClass :: Class a -> MetaClass a

foreign import ccall unsafe "Class.h getClassForObject"
    c_getClassForObject :: Ptr ObjCObject -> IO (Ptr ObjCObject)

getClassForObject obj = withExportedArgument obj c_getClassForObject

{-# NOINLINE unsafeGetMetaclassForClass #-}
unsafeGetMetaclassForClass obj = unsafePerformIO $
    getClassForObject obj >>= importImmortal


class (Object a, Object b) => ClassAndObject a b | a -> b, b -> a

instance ClassAndObject (Class a) (ID a)

class Object cls => ClassObject cls
    where
        classObject :: cls
