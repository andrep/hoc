module Selectors where

import Cocoa

$(declareSelector "sideBarSelection:" [t| forall a. NSTableView a -> IO () |])
