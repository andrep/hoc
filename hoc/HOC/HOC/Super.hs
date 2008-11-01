{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, FlexibleInstances #-}
module HOC.Super(
        SuperClass, SuperTarget, Super(super), withExportedSuper
    ) where

import HOC.Base
import HOC.Arguments
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

data SuperTarget a = SuperTarget a

class Super sub super | sub -> super where
    super :: sub -> super

--- 

pokeSuper objcSuper obj cls
    = pokeByteOff objcSuper 0 obj >> pokeByteOff objcSuper (sizeOf obj) cls

withExportedSuper p action = 
    getSuperClassForObject p >>= \cls ->
    allocaBytes (sizeOf p + sizeOf cls) $ \sptr ->
    pokeSuper sptr p cls >> action sptr

instance MessageTarget a
        => ObjCArgument (SuperTarget a) (Ptr ObjCObject) where

    withExportedArgument (SuperTarget obj) action =
        withExportedArgument obj $ \p ->
        withExportedSuper p action
        
    exportArgument _ = fail "HOC.Super: exportArgument"
    importArgument _ = fail "HOC.Super: importArgument"

    objCTypeString _ = "@"      -- well, close enough.

instance (Object (ID sub), Object super, SuperClass (ID sub) super)
    => Super (ID sub) (SuperTarget super) where
    super obj = SuperTarget (fromID $ toID obj)

getSuperClassForObject obj = do cls <- peekByteOff obj 0 :: IO (Ptr (Ptr ()))
                                peekElemOff cls 1

instance MessageTarget a => MessageTarget (SuperTarget a) where
    isNil (SuperTarget x) = isNil x
    
    sendMessageWithRetval _ = superSendMessageWithRetval
    sendMessageWithoutRetval _ = superSendMessageWithoutRetval
