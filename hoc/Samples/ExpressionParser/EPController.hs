{-# OPTIONS -fglasgow-exts #-}

module EPController where

import Cocoa hiding (parse)
import ExpressionParser
import Selectors
import Text.ParserCombinators.Parsec (parse)

import GHC.Ptr
import HOC.Base

$(declareClass "EPController" "NSObject")

$(exportClass "EPController" "ep_"
  [ Outlet "expressionEntry" [t| NSTextField () |]
  , Outlet "evaluation"      [t| NSTextField () |]
  , InstanceMethod Selectors.info_evaluateExpression
  ]
 )

obj #. var = obj # getIVar var

ep_evaluateExpression _ self = do
  expressionTextField <- (self #. _expressionEntry)
  expression <- expressionTextField # stringValue >>= haskellString
  evaluation <- self #. _evaluation
  case (parse expr "" expression) of
    Left err ->
      self #. _evaluation >>=
        setStringValue (toNSString $ "Error " ++ show err)
    Right answer ->
      self #. _evaluation >>=
        setStringValue (toNSString $ show answer)

