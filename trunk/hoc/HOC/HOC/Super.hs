module HOC.Super(
        SuperClass, SuperTarget, super
    ) where

import HOC.Base
import HOC.Arguments
import HOC.ID
import HOC.MsgSend

import Foreign

{-
    Messages to super.
        [super foo]
    is written as
        super self # foo
-}

class SuperClass sub super | sub -> super

data SuperTarget a = SuperTarget a

super :: (Object sub, Object super, SuperClass sub super)
      => sub -> SuperTarget super

--- 

pokeSuper objcSuper obj cls
    = pokeByteOff objcSuper 0 obj >> pokeByteOff objcSuper (sizeOf obj) cls

instance MessageTarget a
        => ObjCArgument (SuperTarget a) (Ptr ObjCObject) where

    withExportedArgument (SuperTarget obj) action =
        withExportedArgument obj $ \p ->
        getSuperClassForObject p >>= \cls ->
        allocaBytes (sizeOf p + sizeOf cls) $ \sptr ->
        pokeSuper sptr p cls >> action sptr
        
    exportArgument _ = fail "HOC.Super: exportArgument"
    importArgument _ = fail "HOC.Super: importArgument"

    objCTypeString _ = "@"      -- well, close enough.

super obj = SuperTarget (fromID $ toID obj)

getSuperClassForObject obj = do cls <- peekByteOff obj 0 :: IO (Ptr (Ptr ()))
                                peekElemOff cls 1

instance MessageTarget a => MessageTarget (SuperTarget a) where
    isNil (SuperTarget x) = isNil x
    
    sendMessageWithRetval _ = superSendMessageWithRetval
    sendMessageWithStructRetval _ = superSendMessageWithStructRetval
    sendMessageWithoutRetval _ = superSendMessageWithoutRetval
