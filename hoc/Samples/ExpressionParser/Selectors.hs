{-# OPTIONS -fglasgow-exts #-}

module Selectors where

import AppKit.NSButton

$(declareSelector "evaluate:" [t| forall a. NSButton a -> IO () |])

