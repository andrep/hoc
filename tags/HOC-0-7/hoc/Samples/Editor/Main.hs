module Main where

import HaskellDocument(initializeClass_HaskellDocument)

import AppKit.NSApplication(nsApplicationMain_)
    
main = do
    initializeClass_HaskellDocument
    nsApplicationMain_
    
