{-# OPTIONS -fallow-undecidable-instances #-}
module HOC.NewlyAllocated where

{-
    Sending retain and release to objects that have been alloced, but not inited,
    doesn't work in all cases: Sometimes, an init method might decide to
    dealloc the original object --- if the finalizer in ID.hs releases it afterwards,
    we'll crash.
    
    So we introduce a special type for newly allocated objects --- basically
    just an unmanaged pointer. It should only live for a very short time between
    the call to alloc and the call to init.
-}

import HOC.Base         ( ObjCObject )
import HOC.Arguments    ( ObjCArgument(..) )
import HOC.ID           ( Object(..), MessageTarget(..) )
import HOC.MsgSend
import HOC.Super

import Foreign.Ptr      ( Ptr, nullPtr )
import System.IO.Unsafe ( unsafePerformIO )


data NewlyAllocated a
    = NewlyAllocated (Ptr ObjCObject)
    | NewSuper (Ptr ObjCObject)

instance ObjCArgument (NewlyAllocated a) (Ptr ObjCObject) where
    withExportedArgument (NewlyAllocated p) action = action p
    withExportedArgument (NewSuper p) action =
        withExportedSuper p action
    
    exportArgument (NewlyAllocated p) = return p
    exportArgument (NewSuper p) = fail "HOC.NewlyAllocated.NewSuper: exportArgument"
    
    importArgument p = return (NewlyAllocated p)

    objCTypeString _ = "@"

-- Note that NewlyAllocated is not an instance of Object. Objects can be converted
-- to IDs, and IDs are reference counted. Not retaining and releasing objects before
-- they have been inited is the whole point of NewlyAllocated (besides some added type
-- safety)..
    
instance MessageTarget (NewlyAllocated a) where
    isNil (NewlyAllocated p) = p == nullPtr
    isNil (NewSuper p) = p == nullPtr

    sendMessageWithRetval (NewlyAllocated _) = objSendMessageWithRetval
    sendMessageWithRetval (NewSuper _) = superSendMessageWithRetval
    sendMessageWithoutRetval (NewlyAllocated _) = objSendMessageWithoutRetval
    sendMessageWithoutRetval (NewSuper _) = superSendMessageWithoutRetval

instance SuperClass sub super
    => Super (NewlyAllocated sub) (NewlyAllocated super) where
    super (NewlyAllocated x) = NewSuper x
