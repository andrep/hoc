{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, FlexibleInstances,
             FlexibleContexts #-}
module HOC.Super(
        SuperClass, SuperTarget, Super(super), withExportedSuper, castSuper
    ) where

import HOC.Base
import HOC.Arguments
import HOC.Class
import HOC.ID
import HOC.MsgSend
import HOC.MessageTarget

import Foreign

{-
    Messages to super.
        [super foo]
    is written as
        super self # foo
-}

-- This defines the type class for type inheritance.  sub uniquely defines 
-- super, which is sufficient to define a class hierarchy.
class SuperClass sub super | sub -> super

data SuperTarget a = SuperTarget a (Ptr ObjCObject)

class Super sub super | sub -> super where
    super :: sub -> super

--- 

pokeSuper objcSuper obj cls
    = pokeByteOff objcSuper 0 obj >> pokeByteOff objcSuper (sizeOf obj) cls

withExportedSuper p cls action = 
    allocaBytes (sizeOf p + sizeOf cls) $ \sptr ->
    pokeSuper sptr p cls >> action sptr

instance MessageTarget a
        => ObjCArgument (SuperTarget a) (Ptr ObjCObject) where

    withExportedArgument (SuperTarget obj cls) action =
        withExportedArgument obj $ \p ->
        withExportedSuper p cls action
        
    exportArgument _ = fail "HOC.Super: exportArgument"
    importArgument _ = fail "HOC.Super: importArgument"

    objCTypeString _ = "@"      -- well, close enough.

castSuper :: SuperClass (ID sub) (ID super) => ID sub -> ID super
castSuper = castObject

instance (Object (ID sub), Object (ID super), SuperClass (ID sub) (ID super), 
          RawStaticClass (ID super))
    => Super (ID sub) (SuperTarget (ID super)) where
    super obj = SuperTarget (castSuper obj)
                    (rawStaticClassForObject (castSuper obj)) 

instance MessageTarget a => MessageTarget (SuperTarget a) where
    isNil (SuperTarget x cls) = isNil x || cls == nullPtr
    
    sendMessageWithRetval _ = superSendMessageWithRetval
    sendMessageWithoutRetval _ = superSendMessageWithoutRetval
