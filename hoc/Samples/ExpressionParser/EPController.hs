{-# OPTIONS -fglasgow-exts #-}

module EPController where

import Cocoa hiding (parse)
import ExpressionParser
import Selectors
import Text.ParserCombinators.Parsec (parse)

$(declareClass "EPController" "NSObject")

$(exportClass "EPController" "ep_"
  [ Outlet "expressionEntry" [t| NSTextField () |]
  , Outlet "evaluation"      [t| NSTextField () |]
  , InstanceMethod Selectors.info_evaluateExpression
  ]
 )

obj #. var = obj # getIVar var

ep_evaluateExpression _ self = do
expression <- self #. _expressionEntry >>= stringValue >>= haskellString
case (parse expr "" expression) of
  Left e -> 
    self #. _evaluation >>= setStringValue (toNSString $ "Error " ++ show e)
  Right answer ->
    self #. _evaluation >>= setStringValue (toNSString $ show answer)
