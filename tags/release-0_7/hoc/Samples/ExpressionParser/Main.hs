module Main where

import EPController

import AppKit.NSApplication(nsApplicationMain_)
    
main = do
  initializeClass_EPController
  nsApplicationMain_

