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
import HOC.ID           ( Object(..) )

import Foreign.Ptr      ( Ptr )
import System.IO.Unsafe ( unsafePerformIO )

newtype NewlyAllocated a = NewlyAllocated (Ptr ObjCObject)

instance ObjCArgument (NewlyAllocated a) (Ptr ObjCObject) where
    withExportedArgument (NewlyAllocated p) action = action p
    exportArgument (NewlyAllocated p) = return p
    importArgument p = return (NewlyAllocated p)

    objCTypeString _ = "@"

instance Object (NewlyAllocated a) where
    toID (NewlyAllocated p) = unsafePerformIO $ importArgument p
    fromID obj = error "can't convert from ID to NewlyAllocated x"
