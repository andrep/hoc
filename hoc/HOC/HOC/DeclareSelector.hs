module HOC.DeclareSelector where

import HOC.Base
import HOC.Arguments
import HOC.Class
import HOC.Invocation
import HOC.SelectorNameMangling
import HOC.MsgSend
import HOC.FFICallInterface
import HOC.SelectorMarshaller
import HOC.StdArgumentTypes

import Language.Haskell.THSyntax
import Data.Char(isUpper, toLower, toUpper)

data Covariant
data CovariantInstance

$(makeMarshallers 4)
marshallersUpTo = 4

declareRenamedSelector name haskellName typeSigQ =
    do
        typeSig <- typeSigQ
        let
            -- selectorName = "selector_" ++ haskellName
            infoName = "info_" ++ haskellName
            className = "Has_" ++ haskellName
            imptypeName = "ImpType_" ++ haskellName
       
            nArgs = countArgs typeSig
       
            (isPure, pureType) = case resultType typeSig of
                ConT "GHC.IOBase:IO" `AppT` ty -> (False, ty)
                ty -> error $ haskellName ++ " --- selector type must be in the IO monad"
                -- ty -> (True, ty)
    
            isUnit = case pureType of
                ConT "GHC.Base:()" -> True
                _ -> False
    

            doctoredTypeSig = doctorType typeSig className
            
        sequence $ [
                
                -- $(selectorName) = getSelectorForName "name"
                -- valD (VarP selectorName) (normalB [| selectorInfoSel $(infoVar) |]) [],
                
                
                -- $(infoName) = ...
                let e = [| undefined |] `sigE` (return $ simplifyType doctoredTypeSig)
                    in valD (VarP infoName) (normalB
                        [|
                            SelectorInfo name
                                         haskellName
                                         (getCifForSelector $(e))
                                         (getSelectorForName name)
                                         nArgs
                                         isUnit
                        |]) [],
                    
                -- type $(imptypeName) target inst = arg1 -> arg2 -> target -> IO result
                tySynD imptypeName ["target","inst"] (return $ makeImpType typeSig),
                
                -- class Object a => $(className) a
                classD (cxt [conT "Object" `appT` varT "a"]) className ["a"] [],
                
                sigD haskellName $ return doctoredTypeSig,
                
                if nArgs > marshallersUpTo
                    then makeMarshaller (Just infoName) haskellName nArgs isUnit isPure
                    else valD (VarP haskellName) (normalB [|
                            $(varE $ thModulePrefix "DeclareSelector" $
                                    marshallerName nArgs isUnit)
                                $(varE infoName)
                        |]) []
            ]


declareSelector name typeSig = declareRenamedSelector name (mangleSelectorName name) typeSig

resultType (ForallT vars ctxt ty) = resultType ty
resultType ((ArrowT `AppT` _) `AppT` rest) = resultType rest
resultType other = other

countArgs (ForallT vars ctxt ty) = countArgs ty
countArgs ((ArrowT `AppT` _) `AppT` rest) = 1 + countArgs rest
countArgs other = 0

replaceResult new (ForallT vars ctxt ty) = ForallT vars ctxt (replaceResult new ty)
replaceResult new ((ArrowT `AppT` arg) `AppT` rest) =
    (ArrowT `AppT` arg) `AppT` replaceResult new rest
replaceResult new result = new

doctorType ty className = 
        (if needInstance
            then ForallT ["target", "inst"] [ConT className `AppT` VarT "target",
                                            ConT "ClassAndObject"
                                            `AppT` VarT "target" `AppT` VarT "inst"]
            else ForallT ["target"] [ConT className `AppT` VarT "target"]) $
        replaceResult (
            (ArrowT `AppT` VarT "target")
            `AppT` covariantResult
        ) ty
    where
        (needInstance, covariantResult) = doctorCovariant $ resultType ty

doctorCovariant (ConT "HOC.DeclareSelector:Covariant") = (False, VarT "target")
doctorCovariant (ConT "GHC.IOBase:IO" `AppT` ConT "HOC.DeclareSelector:Covariant") =
    (False, ConT "GHC.IOBase:IO" `AppT` VarT "target")
doctorCovariant (ConT "HOC.DeclareSelector:CovariantInstance") = (True, VarT "inst")
doctorCovariant (ConT "GHC.IOBase:IO" `AppT` ConT "HOC.DeclareSelector:CovariantInstance") =
    (True, ConT "GHC.IOBase:IO" `AppT` VarT "inst")
doctorCovariant other = (False, other)

-- Reduce the type to a form that can be used for creating a libffi CIF
-- using the ObjCIMPType type class:

simplifyType (ForallT vars ctxt ty) = simplifyType ty
simplifyType ((ArrowT `AppT` arg) `AppT` rest) = (ArrowT `AppT` replaceVarByUnit arg)
                                                `AppT` simplifyType rest
simplifyType (ConT "GHC.IOBase:IO" `AppT` x) = ConT "GHC.IOBase:IO" `AppT` replaceVarByUnit x
simplifyType x = replaceVarByUnit x

replaceVarByUnit (VarT var) = ConT "HOC.ID:ID" `AppT` ConT "GHC.Base:()"
replaceVarByUnit (ConT cls `AppT` VarT var) = ConT "HOC.ID:ID" `AppT` ConT "GHC.Base:()"
replaceVarByUnit x = x

-- ===

{-
makeImpType (ForallT vars ctxt ty) = makeImpType ty
makeImpType ((ArrowT `AppT` arg) `AppT` (ConT "GHC.IOBase:IO" `AppT` ret)) =
    (ArrowT `AppT` VarT "target") `AppT` (ConT "GHC.IOBase:IO" `AppT` replaceVarByUnit ret)
makeImpType ((ArrowT `AppT` arg) `AppT` rest) = (ArrowT `AppT` replaceVarByUnit arg)
                                                `AppT` makeImpType rest
-}

makeImpType ty = replaceResult (
                    (ArrowT `AppT` VarT "target")
                    `AppT` covariantResult
                ) ty'
    where
        ty' = simplifyType ty
        (_needInstance, covariantResult) = doctorCovariant $ resultType ty'
