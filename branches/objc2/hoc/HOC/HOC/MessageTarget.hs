{-# LANGUAGE FlexibleContexts #-}
module HOC.MessageTarget where

import HOC.Base
import HOC.Arguments
import HOC.ID
import HOC.MsgSend
import HOC.FFICallInterface(FFICif)
import Foreign.Ptr

class ObjCArgument a (Ptr ObjCObject) => MessageTarget a where
    isNil :: a -> Bool
    
    sendMessageWithRetval :: ObjCArgument ret b
                          => a
                          -> FFICif
                          -> Ptr (Ptr ())
                          -> IO ret

    sendMessageWithoutRetval :: a
                             -> FFICif
                             -> Ptr (Ptr ())
                             -> IO ()

class MessageTarget a => Object a where
    toID :: a -> ID ()
    fromID :: ID () -> a

instance MessageTarget (ID a) where
    isNil x = x == nil
    
    sendMessageWithRetval _ = objSendMessageWithRetval
    sendMessageWithoutRetval _ = objSendMessageWithoutRetval

instance Object (ID a) where
    toID (ID a) = ID a
    toID Nil = Nil
    
    fromID (ID a) = ID a
    fromID Nil = Nil

failNilMessage :: MessageTarget t => t -> String -> IO ()
failNilMessage target selectorName
    | isNil target = fail $ "Message sent to nil: " ++ selectorName
    | otherwise = return ()
