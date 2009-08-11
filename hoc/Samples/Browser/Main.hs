module Main where

import Cocoa hiding (main)
import TVUtilities
import BrowserController

import AppKit.NSApplication(nsApplicationMain_)

main = do   
    initializeClass_SimpleTVDataSource
    initializeClass_BrowserController
    nsApplicationMain_
