module HOC.StdArgumentTypes where

import HOC.Base
import HOC.Invocation
import HOC.Arguments
import HOC.FFICallInterface

import Control.Exception        ( bracket )
import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Objective C

-- ID: already defined 

instance FFITypeable SEL where
    makeFFIType _ = makeFFIType (undefined :: Ptr ())

$(declareStorableObjCArgument [t| SEL |] ":")

instance ObjCArgument Bool CChar where
    exportArgument False = return 0
    exportArgument True = return 1
    importArgument 0 = return False
    importArgument _ = return True
    
    objCTypeString _ = "B"    -- OS >= 10.2 only

$(declareStorableObjCArgument [t| Int |] "i")
$(declareStorableObjCArgument [t| Float |] "f")
$(declareStorableObjCArgument [t| Double |] "d")

instance ObjCArgument a b =>
         ObjCArgument (Ptr a) (Ptr a) where
    exportArgument a = return a
    importArgument a = return a
    objCTypeString _
        | nested == "c" = "*"
        | otherwise = '^' : nested
        where nested = objCTypeString (undefined :: a)

-- Foreign.C.Types

$(declareStorableObjCArgument [t| CInt |] "i")
$(declareStorableObjCArgument [t| CUInt |] "I")

$(declareStorableObjCArgument [t| CFloat |] "f")
$(declareStorableObjCArgument [t| CDouble |] "d")

$(declareStorableObjCArgument [t| CChar |] "c")
$(declareStorableObjCArgument [t| CSChar |] "c")
$(declareStorableObjCArgument [t| CUChar |] "C")

$(declareStorableObjCArgument [t| CShort |] "s")
$(declareStorableObjCArgument [t| CUShort |] "S")

$(declareStorableObjCArgument [t| CLong |] "l")
$(declareStorableObjCArgument [t| CULong |] "L")

$(declareStorableObjCArgument [t| CLLong |] "q")
$(declareStorableObjCArgument [t| CULLong |] "Q")

-- String

foreign import ccall safe "Marshalling.h nsStringToUTF8"
    nsStringToUTF8 :: Ptr ObjCObject -> IO CString

foreign import ccall unsafe "Marshalling.h utf8ToNSString"
    utf8ToNSString :: CString -> IO (Ptr ObjCObject)

instance ObjCArgument String (Ptr ObjCObject) where
    withExportedArgument arg action =
        bracket (withCString arg utf8ToNSString) releaseObject action
    exportArgument arg = do
        nsstr <- withCString arg utf8ToNSString
        autoreleaseObject nsstr
        return nsstr
    importArgument arg = nsStringToUTF8 arg >>= peekCString
    
    objCTypeString _ = "*"
