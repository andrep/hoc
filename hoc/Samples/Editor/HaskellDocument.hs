{-# LANGUAGE TemplateHaskell, TypeSynonymInstances,
             FlexibleInstances, MultiParamTypeClasses,
             DeriveDataTypeable #-}
module HaskellDocument where

import HOC

import Cocoa
import Foundation.NSAttributedString (string)
import Foundation.NSString (setString)

import Control.Monad (when)

$(declareClass "HaskellDocument" "NSDocument")

$(exportClass "HaskellDocument" "hd_" [
        Outlet "textView" [t| NSTextView () |],
        InstanceVariable "text" [t| Maybe (NSString ()) |] [| Nothing |],
        
        InstanceMethod 'windowNibName,
        InstanceMethod 'writeToFileOfType,
        InstanceMethod 'readFromFileOfType,
        InstanceMethod 'windowControllerDidLoadNib
    ])
    
hd_windowNibName self =
    return (toNSString "HaskellDocument")

hd_writeToFileOfType :: NSString a -> NSString b -> HaskellDocument () -> IO Bool
hd_writeToFileOfType file typ self = do
    tv <- self # getIVar _textView
    tv # textStorage >>= string >>= writeToFileAtomically file True

hd_readFromFileOfType file typ self = do
    str <- _NSString # alloc >>= initWithContentsOfFile file
    self # setIVar _text (Just str)
    return True

hd_windowControllerDidLoadNib controller self = do
    tv <- self # getIVar _textView
    mbStr <- self # getIVar _text
    case mbStr of 
        Just str -> tv # textStorage >>= mutableString >>= setString str
        Nothing -> return ()

