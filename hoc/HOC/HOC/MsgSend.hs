{-# OPTIONS -cpp #-}
module HOC.MsgSend(
        sendMessageWithRetval,
        sendMessageWithStructRetval,
        sendMessageWithoutRetval
    ) where

import HOC.Base
import HOC.FFICallInterface
import HOC.Arguments
import HOC.Invocation

import Foreign

sendMessageWithRetval :: ObjCArgument a b
                      => FFICif
                      -> Ptr (Ptr ())
                      -> IO a
sendMessageWithStructRetval :: ObjCArgument a b
                            => FFICif
                            -> Ptr (Ptr ())
                            -> IO a
sendMessageWithoutRetval :: FFICif
                         -> Ptr (Ptr ())
                         -> IO ()

#ifdef GNUSTEP

foreign import ccall "objc/objc.h objc_msg_lookup"
    objc_msg_lookup :: Ptr ObjCObject -> SEL -> IO (FunPtr ())
    
    
sendMessageWithRetval cif args = do
    target <- peekElemOff args 0 >>= peek . castPtr
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup target selector
    callWithRetval cif imp args

sendMessageWithStructRetval cif args =
    sendMessageWithRetval cif args

sendMessageWithoutRetval cif args = do
    target <- peekElemOff args 0 >>= peek . castPtr
    selector <- peekElemOff args 1 >>= peek . castPtr
    imp <- objc_msg_lookup target selector
    callWithoutRetval cif imp args

#else

foreign import ccall "objc/objc-runtime.h &objc_msgSend"
    objc_msgSendPtr :: FunPtr (Ptr ObjCObject -> SEL -> IO ())
foreign import ccall "objc/objc-runtime.h &objc_msgSend_stret"
    objc_msgSend_stretPtr :: FunPtr (Ptr a -> Ptr ObjCObject -> SEL -> IO ())

sendMessageWithRetval cif args =
    callWithRetval cif objc_msgSendPtr args

sendMessageWithStructRetval cif args =
    callWithRetval cif objc_msgSend_stretPtr args

sendMessageWithoutRetval cif args =
    callWithoutRetval cif objc_msgSendPtr args

#endif
