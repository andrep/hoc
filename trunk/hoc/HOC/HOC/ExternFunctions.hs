{-# LANGUAGE TemplateHaskell #-}
module HOC.ExternFunctions(declareExternFun) where

import HOC.TH
import HOC.Arguments
import HOC.Invocation
import HOC.Dyld
import HOC.NameCaseChange

import Foreign
import System.IO.Unsafe

declareExternFun :: String -> TypeQ -> Q [Dec]

declareExternFun name typeSigQ
    = do
        typeSig <- typeSigQ

        let
            n = mkName $ nameToLowercase name
            cifN = mkName $ "cif__" ++ name
            ptrN = mkName $ "c__" ++ name
            
                -- ### FIXME: Code Duplication from SelectorMarshaller.hs
    
            arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
    
            argumentsToMarshal = map (varE.mkName) arguments
            marshalledArguments = map (mkName . (++"'")) arguments
       
            marshallerBody = marshallArgs  $
                             collectArgs $
                             invoke
    
            marshallArgs = marshallArgs' argumentsToMarshal marshalledArguments
                where
                    marshallArgs' [] [] e = e
                    marshallArgs' (arg:args) (arg':args') e =
                        [| withMarshalledArgument $(arg)
                                                  $(lamE [varP arg'] e') |]
                        where e' = marshallArgs' args args' e
       
            collectArgs e = [| withArray
                                $(listE (map varE marshalledArguments))
                                $(lamE [varP $ mkName "args"] e) |]
    
            invoke | isUnit = [| callWithoutRetval $(varE cifN) $(varE ptrN)
                                                   $(varE $ mkName "args")|]
                   | otherwise = [| callWithRetval $(varE cifN) $(varE ptrN)
                                                   $(varE $ mkName "args")|]
    
                -- ### FIXME: Code Duplication from DeclareSelector.hs
            countArgs (ForallT vars ctxt ty) = countArgs ty
            countArgs ((ArrowT `AppT` _) `AppT` rest) = 1 + countArgs rest
            countArgs other = 0 :: Int
    
            resultType (ForallT vars ctxt ty) = resultType ty
            resultType ((ArrowT `AppT` _) `AppT` rest) = resultType rest
            resultType other = other
    
            (isPure, pureType) = case resultType typeSig of
                (ConT con) `AppT` ty
                    | con == ''IO -> (False, ty)
                ty -> error $ name ++ " --- function type must be in the IO monad"
                -- ty -> (True, ty)
        
            isUnit = pureType == ConT ''()
            nArgs = countArgs typeSig
            
        -- in
        sequence [
                sigD n typeSigQ,
                
                valD (varP cifN)
                     (normalB [| getCifForFunction $(varE n) |]) [],
                valD (varP ptrN)
                     (normalB [| unsafePerformIO $
                                 lookupSymbol $(stringE name) |]) [],

                funD n [
                    clause (map (varP.mkName) arguments)
                           (normalB $ marshallerBody)
                           []
                ]
            ]
