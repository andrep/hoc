{-# OPTIONS -fglasgow-exts #-}

module EPController where

import Cocoa hiding (parse, info_evaluate, ImpType_evaluate)
import ExpressionParser
import Selectors
import Text.ParserCombinators.Parsec (parse)

$(declareClass "EPController" "NSObject")

$(exportClass "EPController" "ep_"
  [ Outlet "expressionEntry" [t| NSTextField () |]
  , Outlet "evaluation"      [t| NSTextField () |]
  , InstanceMethod Selectors.info_evaluate
  ]
 )

obj #. var = obj # getIVar var

ep_evaluate _ self = do
  expressionTextField <- (self #. _expressionEntry)
  expression <- expressionTextField # stringValue >>= haskellString
  evaluation <- self #. _evaluation
  case (parse expr "" expression) of
    Left err ->
      self #. _evaluation >>= setStringValue (toNSString "(error)")
    Right answer ->
      self #. _evaluation >>= setStringValue (toNSString $ show answer)

