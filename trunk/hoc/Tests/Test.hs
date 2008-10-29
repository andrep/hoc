module Main where

import qualified TestFFI
import qualified TestPreprocessor

import Test.HUnit

main = runTestTT $ test [
        TestFFI.tests,
        TestPreprocessor.tests
    ]
