module HOC.SelectorMarshaller(
        SelectorInfo(..),
        makeMarshaller,
        makeMarshallers,
        marshallerName
    ) where

import HOC.Base
import HOC.Arguments
import HOC.ID
import HOC.Class
import HOC.Invocation
import HOC.SelectorNameMangling
import HOC.MsgSend
import HOC.FFICallInterface

import Foreign                      ( withArray, Ptr, nullPtr )
import System.IO.Unsafe             ( unsafePerformIO )

import HOC.TH

data SelectorInfo = SelectorInfo {
        selectorInfoObjCName :: String,
        selectorInfoHaskellName :: String,
        selectorInfoCif :: FFICif,
        selectorInfoSel :: SEL,
        selectorInfoNArgs :: Int,
        selectorInfoIsUnit :: Bool
    }

makeMarshaller maybeInfoName haskellName nArgs isUnit isPure isRetained =
            funD haskellName [
                clause (map varP $ infoArgument ++ map mkName arguments
                        ++ [mkName "target"])
                     (normalB $ marshallerBody
                    ) []
            ]
    where
        (infoVar, infoArgument) = case maybeInfoName of
                    Just name -> (varE name, [])
                    Nothing -> (varE (mkName "info"), [mkName "info"])
        arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
        argumentsToMarshal = varE (mkName "target")
                           : [| selectorInfoSel $(infoVar) |]
                           : map (varE.mkName) arguments
        marshalledArguments = mkName "target'"
                            : mkName "selector'"
                            : map (mkName . (++"'")) arguments
   
        marshallerBody = purify $
                         checkTargetNil $
                         releaseRetvalIfRetained $
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
                                     $(lamE [varP $ mkName "args"] e) |]

        invoke | isUnit = [| sendMessageWithoutRetval (selectorInfoCif $(infoVar))
                                                      $(argsVar)|]
               | otherwise = [| sendMessageWithRetval (selectorInfoCif $(infoVar))
                                                      $(argsVar)|]
            where argsVar = varE $ mkName "args"

        purify e | isPure = [| unsafePerformIO $(e) |]
                 | otherwise = e
                 
        releaseRetvalIfRetained e | isRetained = [| $(e) >>= releaseExtraReference |]
                                  | otherwise = e
                                  
        checkTargetNil e = [| failNilMessage $(varE $ mkName "target")
                                             (selectorInfoHaskellName $(infoVar))
                              >> $(e) |]
    
makeMarshallers n =
        sequence $
        [ makeMarshaller Nothing (mkName $ marshallerName nArgs isUnit) nArgs isUnit False False
        | nArgs <- [0..n], isUnit <- [False, True] ]

marshallerName nArgs False = "method" ++ show nArgs
marshallerName nArgs True = "method" ++ show nArgs ++ "_"
