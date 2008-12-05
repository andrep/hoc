module Main where

import qualified TestFFI
import qualified TestPreprocessor
import qualified TestFoundation

import Test.HUnit

import HOC.Base( withAutoreleasePool )

main = withAutoreleasePool $ runTestTT $ test [
        TestFFI.tests,
        TestPreprocessor.tests,
        TestFoundation.tests
    ]
