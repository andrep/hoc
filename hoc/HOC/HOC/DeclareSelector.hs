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
import HOC.Super
import HOC.CannedCIFs
import HOC.MessageTarget

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

{-# NOINLINE method0 #-} 
{-# NOINLINE method0_ #-}
{-# NOINLINE method1 #-}
{-# NOINLINE method1_ #-}
{-# NOINLINE method2 #-}
{-# NOINLINE method2_ #-}
{-# NOINLINE method3 #-}
{-# NOINLINE method3_ #-}
{-# NOINLINE method4 #-}
{-# NOINLINE method4_ #-}

$(makeCannedCIFs [
        [t| ID () -> IO () |],
        [t| ID () -> IO (ID ()) |],
        [t| ID () -> IO Bool |],
        [t| ID () -> IO Float |],
        [t| ID () -> IO Double |],
        [t| ID () -> ID () -> IO () |],
        [t| ID () -> ID () -> IO (ID ()) |],
        [t| Bool -> ID () -> IO () |],
        [t| Float -> ID () -> IO () |],
        [t| Double -> ID () -> IO () |],
        [t| ID () -> ID () -> IO Bool |],
        [t| ID () -> ID () -> IO Float |],
        [t| ID () -> ID () -> IO Double |],
        [t| ID () -> ID () -> ID () -> IO () |],
        [t| ID () -> ID () -> ID () -> IO (ID ()) |],
        [t| ID () -> ID () -> ID () -> IO Bool |],
        [t| ID () -> ID () -> ID () -> ID () -> IO () |],
        [t| ID () -> ID () -> ID () -> ID () -> IO (ID ()) |],
        [t| ID () -> ID () -> ID () -> ID () -> IO Bool |]
    ])

declareRenamedSelector name haskellName typeSigQ =
    do
        typeSig <- typeSigQ
        let
            -- selectorName = "selector_" ++ haskellName
            infoName = "info_" ++ haskellName
            className = "Has_" ++ haskellName
            imptypeName = "ImpType_" ++ haskellName
       
            nArgs = countArgs typeSig
            
            -- isPure is a boolean that indicates if the resultType of our type
            -- signature is not in the IO mondad
            -- 
            -- pureType is the type that was used to initalize IO
            -- 
            -- all selectors must be in the IO monad.
            (isPure, pureType) = case resultType typeSig of
                (ConT con) `AppT` ty
                    | con == ''IO -> (False, ty)
                    | otherwise -> badType
                ty -> badType
                where
                    badType = error $ haskellName ++ " --- selector type must be in the IO monad"
  
            -- isUnit is a boolean which is true if pureType is unit.  This 
            -- will eventually be used by makeMarsheller and marshellerName
            isUnit = pureType == ConT ''()

            (resultRetained, doctoredTypeSig) = doctorType typeSig className
            
            -- resultType --
            -- given a type, returns the return value, all applications
            resultType (ForallT vars ctxt ty) = resultType ty
            resultType ((ArrowT `AppT` _) `AppT` rest) = resultType rest
            resultType other = other
            
            -- countArgs
            -- return the number of arguments that the function takes.
            countArgs (ForallT vars ctxt ty) = countArgs ty
            countArgs ((ArrowT `AppT` _) `AppT` rest) = 1 + countArgs rest
            countArgs other = 0
            
            -- substitute the result of the second argument for the first. 
            replaceResult new (ForallT vars ctxt ty) = ForallT vars ctxt (replaceResult new ty)
            replaceResult new ((ArrowT `AppT` arg) `AppT` rest) =
                (ArrowT `AppT` arg) `AppT` replaceResult new rest
            replaceResult new result = new

            -- this takes:
            -- forall <names> (context). (forall <names>' (context'). type')
            -- and turns it into
            -- forall <names> ++ <names>' (context ++ context'). type'
            -- Thus it "flattens" foralls
            liftForalls (ForallT names cxt ty)
                = case liftForalls ty of
                    ForallT names' cxt' ty'
                        -> ForallT (names ++ names') (cxt ++ cxt') ty'
                    ty' -> ForallT names cxt ty'
            liftForalls other = other
            
            -- this takes a type and a class name and produces another type:
            -- forall target instance.
            --     (className target, ClassAndObject target instance) .
            --     (target-> covarientResult)
            --  
            doctorType ty className = 
                    (
                        retained,
                        liftForalls $
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
            -- 
            -- doctorCovariant --
            -- The values returned in the 4-tuple are:
            -- retained: true if this object has been retained after this 
            -- method call
            -- needInstance: true if this method needs an instance passed in 
            -- (eg. is it "static")
            -- targetType:
            -- covarientResult: TODO
            --
            -- the first form handles almost all the types that deal with 
            -- reference count types.  Retained is handled below
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
                                (ArrowT `AppT` fromMaybe (VarT $ mkName "target") target')
                                `AppT` covariantResult
                            ) ty'
                where
                    ty' = simplifyType ty
                    (_retained, _needInstance, target', covariantResult) =
                        doctorCovariant $ resultType ty'
            
            selInfoMaker | resultRetained = [| mkSelectorInfoRetained |]
                         | otherwise      = [| mkSelectorInfo |]
            
        sequence $ [
                
                -- $(selectorName) = getSelectorForName "name"
                -- valD (VarP selectorName) (normalB [| selectorInfoSel $(infoVar) |]) [],
                
                
                -- $(infoName) = ...
                let e = [| undefined |] `sigE` (return $ simplifyType doctoredTypeSig)
                    in valD (varP $ mkName $ infoName) (normalB
                        [|
                        	let n = $(stringE name)
                        	in $(selInfoMaker) n
                                                $(if haskellName == name
                                                        then [|n|]
                                                        else stringE haskellName)
                                                $(staticCifForSelectorType
                                                        'marshallersUpTo
                                                        cannedCIFTypeNames
                                                        (return $ simplifyType doctoredTypeSig))
                        |]) [],
                    
                -- type $(imptypeName) target inst = arg1 -> arg2 -> target -> IO result
                tySynD (mkName imptypeName) (map mkName ["target","inst"])
                    (return $ makeImpType typeSig),
                
                -- class Object a => $(className) a
                classD (cxt [conT ''MessageTarget `appT` varT (mkName "a")])
                    (mkName className) [mkName "a"] [] [],
                
                -- instance $(className) a => $(className) (SuperTarget a)
                instanceD (cxt [conT (mkName className) `appT` varT (mkName "a")])
                    (conT (mkName className) `appT` (conT ''SuperTarget `appT` varT (mkName "a")))
                    [],
                
                sigD (mkName haskellName) $ return doctoredTypeSig,
                
                if nArgs > marshallersUpTo || resultRetained
                    then makeMarshaller (Just $ mkName infoName) (mkName haskellName) nArgs
                                        isUnit isPure resultRetained
                    else valD (varP $ mkName haskellName) (normalB [|
                            $(varE $ 
                                    marshallerName nArgs isUnit `fromSameModuleAs_v`
                                    	'marshallersUpTo
                                )
                                $(varE $ mkName infoName)
                        |]) []
            ]

declareSelector name typeSig = declareRenamedSelector name (mangleSelectorName name) typeSig

