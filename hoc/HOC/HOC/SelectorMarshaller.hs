module HOC.SelectorMarshaller where

import HOC.Base
import HOC.Arguments
import HOC.Class
import HOC.Invocation
import HOC.SelectorNameMangling
import HOC.MsgSend
import HOC.FFICallInterface

import Language.Haskell.THSyntax
import Foreign(withArray)
import System.IO.Unsafe(unsafePerformIO)

data SelectorInfo = SelectorInfo {
        selectorInfoObjCName :: String,
        selectorInfoHaskellName :: String,
        selectorInfoCif :: FFICif,
        selectorInfoSel :: SEL,
        selectorInfoNArgs :: Int,
        selectorInfoIsUnit :: Bool
    }

makeMarshaller maybeInfoName haskellName nArgs isUnit isPure =
            funD haskellName [
                clause (map VarP $ infoArgument ++ arguments
                        ++ ["target"])
                     (normalB $ marshallerBody
                    ) []
            ]
    where
        (infoVar, infoArgument) = case maybeInfoName of
                    Just name -> (varE name, [])
                    Nothing -> (varE "info", ["info"])
        arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
        argumentsToMarshal = varE "target"
                           : [| selectorInfoSel $(infoVar) |]
                           : map varE arguments
        marshalledArguments = "target'" : "selector'" : map (++"'") arguments
   
        marshallerBody = purify $
                         marshallArgs  $
                         collectArgs $
                         invoke

        marshallArgs = marshallArgs' argumentsToMarshal marshalledArguments
            where
                marshallArgs' [] [] e = e
                marshallArgs' (arg:args) (arg':args') e =
                    [| withMarshalledArgument $(arg) $(lamE [varP arg'] e') |]
                    where e' = marshallArgs' args args' e
   
        collectArgs e = [| withArray $(listE (map varE marshalledArguments))
                                     $(lamE [varP "args"] e) |]

        invoke | isUnit = [| callWithoutRetval (selectorInfoCif $(infoVar)) objc_msgSendPtr $(argsVar) |]
               | otherwise = [| callWithRetval (selectorInfoCif $(infoVar)) objc_msgSendPtr $(argsVar) |]
            where argsVar = varE "args"

        purify e | isPure = [| unsafePerformIO $(e) |]
                 | otherwise = e
    
makeMarshallers n =
        sequence $
        [ makeMarshaller Nothing (marshallerName nArgs isUnit) nArgs isUnit False
        | nArgs <- [0..n], isUnit <- [False, True] ]

marshallerName nArgs False = "method" ++ show nArgs
marshallerName nArgs True = "method" ++ show nArgs ++ "_"
