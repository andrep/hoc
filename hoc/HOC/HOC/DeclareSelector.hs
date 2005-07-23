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
import HOC.ID
import HOC.NewlyAllocated(NewlyAllocated)

import Data.Char(isUpper, toLower, toUpper)
import Data.Maybe(fromMaybe)
import Control.Monad(MonadPlus(mplus))

import HOC.TH

data Covariant
data CovariantInstance
data Allocated
data Inited
data Retained a

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
                (ConT con) `AppT` ty
                    | con == ''IO -> (False, ty)
                ty -> error $ haskellName ++ " --- selector type must be in the IO monad"
                -- ty -> (True, ty)
    
            isUnit = pureType == ConT ''()

            (resultRetained, doctoredTypeSig) = doctorType typeSig className
            
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
                    (
                        retained,
                        (if needInstance
                            then ForallT (map mkName ["target", "inst"])
                                [ConT (mkName className) `AppT` VarT (mkName "target"),
                                 ConT (mkName "ClassAndObject")
                                    `AppT` VarT (mkName "target") `AppT` VarT (mkName "inst")]
                            else ForallT [mkName "target"]
                                [ConT (mkName className) `AppT` VarT (mkName "target")]) $
                        replaceResult (
                            (ArrowT `AppT` (fromMaybe (VarT $ mkName "target") targetType))
                            `AppT` covariantResult
                        ) ty
                    )
                where
                    (retained, needInstance, targetType, covariantResult) =
                        doctorCovariant $ resultType ty

            doctorCovariant (ConT con)
                | con == ''Covariant =
                    (False, False, Nothing, VarT $ mkName "target")
                | con == ''CovariantInstance =
                    (False, True, Nothing, VarT $ mkName "inst")
                | con == ''Allocated =
                    (False, True, Nothing,
                        ConT ''NewlyAllocated `AppT` VarT (mkName "inst"))
                | con == ''Inited =
                    (True, False,
                        Just (ConT ''NewlyAllocated `AppT` VarT (mkName "target")),
                                    VarT (mkName "target"))
                                    
            doctorCovariant (ConT con `AppT` ty) | con == ''Retained =
                    (True,inst', target', ty')
                where (_,inst', target', ty') = doctorCovariant ty

            doctorCovariant (t1 `AppT` t2) =
                    (retained1 || retained2, needInst1 || needInst2, target1 `mplus` target2, t1' `AppT` t2')
                where (retained1, needInst1, target1, t1') = doctorCovariant t1
                      (retained2, needInst2, target2, t2') = doctorCovariant t2

            doctorCovariant other = (False, False, Nothing, other)

            -- Reduce the type to a form that can be used for creating a libffi CIF
            -- using the ObjCIMPType type class:

            simplifyType (ForallT vars ctxt ty) = simplifyType ty
            simplifyType ((ArrowT `AppT` arg) `AppT` rest) = (ArrowT `AppT` replaceVarByUnit arg)
                                                            `AppT` simplifyType rest
            simplifyType (ConT con `AppT` x) | con == ''IO = ConT ''IO `AppT` replaceVarByUnit x
            simplifyType x = replaceVarByUnit x

            replaceVarByUnit (VarT var) = ConT ''ID `AppT` ConT ''()
            replaceVarByUnit (ConT con `AppT` ty)
                | con == ''NewlyAllocated = replaceVarByUnit ty
            replaceVarByUnit (ConT cls `AppT` VarT var) = ConT cls `AppT` ConT ''()
            replaceVarByUnit x = x


            makeImpType ty = replaceResult (
                                (ArrowT `AppT` VarT (mkName "target"))
                                `AppT` covariantResult
                            ) ty'
                where
                    ty' = simplifyType ty
                    (_retained, _needInstance, _target', covariantResult) =
                        doctorCovariant $ resultType ty'
        
        sequence $ [
                
                -- $(selectorName) = getSelectorForName "name"
                -- valD (VarP selectorName) (normalB [| selectorInfoSel $(infoVar) |]) [],
                
                
                -- $(infoName) = ...
                let e = [| undefined |] `sigE` (return $ simplifyType doctoredTypeSig)
                    in valD (varP $ mkName $ infoName) (normalB
                        [|
                        	let n = $(stringE name)
                        	in SelectorInfo n
                                                $(if haskellName == name
                                                        then [|n|]
                                                        else stringE haskellName)
                                                (getCifForSelector $(e))
                                                (getSelectorForName n)
                                                nArgs
                                                isUnit
                        |]) [],
                    
                -- type $(imptypeName) target inst = arg1 -> arg2 -> target -> IO result
                tySynD (mkName imptypeName) (map mkName ["target","inst"])
                    (return $ makeImpType typeSig),
                
                -- class Object a => $(className) a
                classD (cxt [conT (mkName "Object") `appT` varT (mkName "a")])
                    (mkName className) [mkName "a"] [] [],
                
                sigD (mkName haskellName) $ return doctoredTypeSig,
                
                if nArgs > marshallersUpTo || resultRetained
                    then makeMarshaller (Just $ mkName infoName) (mkName haskellName) nArgs
                                        isUnit isPure resultRetained
                    else valD (varP $ mkName haskellName) (normalB [|
                            $(varE $ 
                                    mkNameG_v "HOC.DeclareSelector" $
                                    marshallerName nArgs isUnit
                                )
                                $(varE $ mkName infoName)
                        |]) []
            ]

declareSelector name typeSig = declareRenamedSelector name (mangleSelectorName name) typeSig

