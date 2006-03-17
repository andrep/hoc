{-# OPTIONS -cpp #-}
module HOC.MsgSend(
        objSendMessageWithRetval,
        objSendMessageWithoutRetval,
        superSendMessageWithRetval,
        superSendMessageWithoutRetval
    ) where

import HOC.Base
import HOC.FFICallInterface
import HOC.Arguments
import HOC.Invocation

import Foreign
import Control.Monad.Fix(mfix)

objSendMessageWithRetval
	:: ObjCArgument a b
    => FFICif
    -> Ptr (Ptr ())
    -> IO a

objSendMessageWithoutRetval
	:: FFICif
    -> Ptr (Ptr ())
    -> IO ()

superSendMessageWithRetval
	:: ObjCArgument a b
    => FFICif
    -> Ptr (Ptr ())
    -> IO a

superSendMessageWithoutRetval
	:: FFICif
    -> Ptr (Ptr ())
    -> IO ()

#ifdef GNUSTEP

foreign import ccall "objc/objc.h objc_msg_lookup"
    objc_msg_lookup :: Ptr ObjCObject -> SEL -> IO (FunPtr ())

foreign import ccall "objc/objc.h objc_msg_lookup_super"
    objc_msg_lookup_super :: Ptr ObjCObject -> SEL -> IO (FunPtr ())
     
sndMsgCommon call cif args = do
    target <- peekElemOff args 0 >>= peek . castPtr
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup target selector
    call cif imp args
    
sndMsgSuperCommon call cif args = do
    super <- peekElemOff args 0 >>= peek . castPtr
    peek (castPtr super) >>= pokeElemOff args 0
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup_super super selector
    call cif imp args

    
objSendMessageWithRetval      = sndMsgCommon      callWithRetval
objSendMessageWithoutRetval   = sndMsgCommon      callWithoutRetval
superSendMessageWithRetval    = sndMsgSuperCommon callWithRetval
superSendMessageWithoutRetval = sndMsgSuperCommon callWithoutRetval

#else

	-- the type signatures are essentially bogus
	-- the return value is not necessarily (), and might even be a struct.
	-- we only call them via libffi, so we couldn't care less.
foreign import ccall "MsgSend.h &objc_msgSend"
    objc_msgSendPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())
foreign import ccall "MsgSend.h &objc_msgSend_stret"
    objc_msgSend_stretPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())

foreign import ccall "MsgSend.h &objc_msgSendSuper"
    objc_msgSendSuperPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())
foreign import ccall "MsgSend.h &objc_msgSendSuper_stret"
    objc_msgSendSuper_stretPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())

withMarshalledDummy :: ObjCArgument a b => (b -> IO a) -> IO a
withMarshalledDummy action = action undefined

objSendMessageWithRetval cif args =
	withMarshalledDummy $ \dummy ->
	cifIsStret cif >>= \isStret ->
	callWithRetval cif (if isStret /= 0
                                then objc_msgSend_stretPtr
                                else objc_msgSendPtr) args    	 

objSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendPtr args


superSendMessageWithRetval cif args =
	withMarshalledDummy $ \dummy ->
	cifIsStret cif >>= \isStret ->
	callWithRetval cif (if isStret /= 0
                                then objc_msgSendSuper_stretPtr
                                else objc_msgSendSuperPtr) args    	 

superSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendSuperPtr args

#endif
