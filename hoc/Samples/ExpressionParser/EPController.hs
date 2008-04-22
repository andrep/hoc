{-# OPTIONS_GHC -fglasgow-exts -fth #-}

module EPController where

import Cocoa hiding (parse)
import ExpressionParser
import Text.ParserCombinators.Parsec (parse)

$(declareClass "EPController" "NSObject")

$(declareSelector "evaluateExpression:" [t| forall a. NSButton a -> IO () |])

$(exportClass "EPController" "ep_"
  [ Outlet "expressionEntry" [t| NSTextField () |]
  , Outlet "evaluation"      [t| NSTextField () |]
  , InstanceMethod 'evaluateExpression
  ]
 )

-- obj #. var = obj # getIVar var

ep_evaluateExpression _ self = do
expression <- self #. _expressionEntry >>= stringValue >>= haskellString
case (parse expr "" expression) of
  Left e -> 
    self #. _evaluation >>= setStringValue (toNSString $ "Error " ++ show e)
  Right answer ->
    self #. _evaluation >>= setStringValue (toNSString $ show answer)
