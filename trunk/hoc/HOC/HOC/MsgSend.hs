{-# OPTIONS -cpp #-}
module HOC.MsgSend(
        objSendMessageWithRetval,
        objSendMessageWithStructRetval,
        objSendMessageWithoutRetval,
        superSendMessageWithRetval,
        superSendMessageWithStructRetval,
        superSendMessageWithoutRetval
    ) where

import HOC.Base
import HOC.FFICallInterface
import HOC.Arguments
import HOC.Invocation

import Foreign

objSendMessageWithRetval
	:: ObjCArgument a b
    => FFICif
    -> Ptr (Ptr ())
    -> IO a

objSendMessageWithStructRetval
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

superSendMessageWithStructRetval
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
    
    
objSendMessageWithRetval cif args = do
    target <- peekElemOff args 0 >>= peek . castPtr
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup target selector
    callWithRetval cif imp args

objSendMessageWithStructRetval cif args =
    objSendMessageWithRetval cif args

objSendMessageWithoutRetval cif args = do
    target <- peekElemOff args 0 >>= peek . castPtr
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup target selector
    callWithoutRetval cif imp args

#error GNUSTEP unimplemented: send message to super

#else

foreign import ccall "MsgSend.h &objc_msgSend"
    objc_msgSendPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())
foreign import ccall "MsgSend.h &objc_msgSend_stret"
    objc_msgSend_stretPtr :: FunPtr (Ptr a -> Ptr ObjCObject -> SEL -> IO ())

foreign import ccall "MsgSend.h &objc_msgSendSuper"
    objc_msgSendSuperPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())
foreign import ccall "MsgSend.h &objc_msgSendSuper_stret"
    objc_msgSendSuper_stretPtr :: FunPtr (Ptr a -> Ptr ObjCObject -> SEL -> IO ())

objSendMessageWithRetval cif args =
    callWithRetval cif objc_msgSendPtr args

objSendMessageWithStructRetval cif args =
    callWithRetval cif objc_msgSend_stretPtr args

objSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendPtr args


superSendMessageWithRetval cif args =
    callWithRetval cif objc_msgSendSuperPtr args

superSendMessageWithStructRetval cif args =
    callWithRetval cif objc_msgSendSuper_stretPtr args

superSendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendSuperPtr args

#endif
