module Selectors where

import HOC

import AppKit.NSTextView.Forward

$(declareSelector "setTextView" [t| forall a. NSTextView a -> IO () |])
