{-# OPTIONS -fglasgow-exts #-}

module Selectors where

import AppKit.NSButton

$(declareSelector "evaluateExpression:" [t| forall a. NSButton a -> IO () |])

