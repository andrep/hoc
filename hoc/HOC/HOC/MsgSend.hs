module HOC.MsgSend where

import HOC.Base

import Foreign

foreign import ccall "objc/objc-runtime.h &objc_msgSend"
    objc_msgSendPtr :: FunPtr (ObjCObject -> SEL -> IO ())
