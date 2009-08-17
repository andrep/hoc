{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    ScopedTypeVariables #-}
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
import HOC.Class
import HOC.ID
import HOC.MessageTarget( MessageTarget(..) )
import HOC.MsgSend
import HOC.Super

import Foreign.Ptr      ( Ptr, nullPtr )

data NewlyAllocated a
    = NewlyAllocated (Ptr ObjCObject)
    | NewSuper (Ptr ObjCObject) (Class ())

instance ObjCArgument (NewlyAllocated a) (Ptr ObjCObject) where
    withExportedArgument (NewlyAllocated p) action = action p
    withExportedArgument (NewSuper p cls) action =
        withExportedArgument cls $ \cls ->
        withExportedSuper p cls action
    
    exportArgument (NewlyAllocated p) = return p
    exportArgument (NewSuper p cls) = fail "HOC.NewlyAllocated.NewSuper: exportArgument"
    
    importArgument p = return (NewlyAllocated p)

    objCTypeString _ = "@"

-- Note that NewlyAllocated is not an instance of Object. Objects can be converted
-- to IDs, and IDs are reference counted. Not retaining and releasing objects before
-- they have been inited is the whole point of NewlyAllocated (besides some added type
-- safety)..
    
instance MessageTarget (NewlyAllocated a) where
    isNil (NewlyAllocated p) = p == nullPtr
    isNil (NewSuper p cls) = (p == nullPtr) || isNil cls

    sendMessageWithRetval (NewlyAllocated _) = objSendMessageWithRetval
    sendMessageWithRetval (NewSuper _ _) = superSendMessageWithRetval
    sendMessageWithoutRetval (NewlyAllocated _) = objSendMessageWithoutRetval
    sendMessageWithoutRetval (NewSuper _ _) = superSendMessageWithoutRetval

instance (SuperClass sub (ID super), ClassObject (Class super))
    => Super (NewlyAllocated sub) (NewlyAllocated (ID super)) where
    super (NewlyAllocated x) = NewSuper x (castObject (classObject :: Class super))
