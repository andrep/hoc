module Main where

import Cocoa
import TVUtilities
import BrowserController

main = do   
    initializeClass_SimpleTVDataSource
    initializeClass_BrowserController
    nsApplicationMain_
