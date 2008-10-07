{-# OPTIONS -fglasgow-exts -fth #-}
module HOC.CStruct( declareCStruct, declareCStructWithTag ) where
    
import HOC.Arguments        ( ObjCArgument(..) )
import HOC.TH
import HOC.NameCaseChange   ( nameToUppercase )
import HOC.FFICallInterface

import Control.Monad.State
import Data.Bits
import Data.Maybe           ( fromMaybe )
import Foreign

declareCStruct :: String -> [TypeQ] -> Q [Dec]
declareCStructWithTag :: String -> Maybe String -> [TypeQ] -> Q [Dec]


mkRawThing :: ObjCArgument a b => a -> b
mkRawThing _ = undefined

sizeMember :: ObjCArgument a b => a -> State Int ()
sizeMember thing =
    modify (\offset -> align offset (alignment rawThing) + sizeOf rawThing)

    where align x a = (x + (a-1)) .&. complement (a-1)
          rawThing = mkRawThing thing

alignMember :: ObjCArgument a b => a -> Int
alignMember = alignment . mkRawThing

pokeMember :: ObjCArgument a b => a -> StateT (Ptr c) IO ()
pokeMember thing = do
    rawThing <- lift $ exportArgument thing
    modify (`alignPtr` alignment rawThing)
    p <- get
    lift $ poke (castPtr p) rawThing
    modify (`plusPtr` sizeOf rawThing)

peekMember :: ObjCArgument a b => StateT (Ptr c) IO a
peekMember = (mfix $ \result -> do
    modify (`alignPtr` alignment result)
    p <- get
    rawThing <- lift $ peek (castPtr p)
    modify (`plusPtr` sizeOf rawThing)
    return rawThing) >>= \rawThing -> lift (importArgument rawThing)
    
ffiMember :: ObjCArgument a b => a -> StateT [FFIType] IO ()
ffiMember thing = do
    t <- lift $ makeFFIType (mkRawThing thing)
    modify (t :)


declareCStruct cname fieldTypes
    = declareCStructWithTag cname Nothing fieldTypes

declareCStructWithTag cname mbTag fieldTypes
    = do
        let name = mkName $ nameToUppercase cname
            structTag = fromMaybe "?" mbTag
        dataDecl <- dataD (cxt []) name [] [
                normalC name $
                    map (strictType (return NotStrict)) fieldTypes
            ] [''Eq, ''Ord] --, ''Read, ''Show]
        
        varNames <- mapM (const $ newName "field") fieldTypes
        ptrName <- newName "ptr"
    
        let takeApartP = conP name $ map varP varNames
            putTogetherE | null varNames = conE name
                         | otherwise = appsE $ (conE name : map varE varNames)

            doWithArgs name | null varNames = [| return () |]
            doWithArgs name = doE $
                [ noBindS (varE name `appE` varE field)
                | field <- varNames ]
                
            doWithResults name = doE $
                [ bindS (varP field) (varE name)
                | field <- varNames ]
                ++ [ noBindS [| return $(putTogetherE) |] ]

            mapArgs name = listE $
                [ varE name `appE` varE field | field <- varNames ]

        storableDecl <- instanceD (cxt []) (conT ''Storable `appT` conT name)
            [
                funD 'alignment [
                    clause [tildeP takeApartP]
                        (normalB [| maximum ( 1 : $(mapArgs 'alignMember) ) |]) []
                ],
                funD 'sizeOf [
                    clause [tildeP takeApartP] 
                        (normalB [| execState $(doWithArgs 'sizeMember) 0 |])
                        []
                ],
                funD 'poke [
                    clause [varP ptrName, takeApartP] 
                        (normalB [| evalStateT $(doWithArgs 'pokeMember) $(varE ptrName) |])
                        []
                ],
                funD 'peek [
                    clause [varP ptrName]
                        (normalB [| evalStateT $(doWithResults 'peekMember) $(varE ptrName) |])
                        []
                ]
            ]
    
        ffiDecl <- instanceD (cxt []) (conT ''FFITypeable `appT` conT name)
            [
                funD 'isStructType [ clause [wildP] (normalB [| True |]) [] ],
                funD 'makeFFIType [
                    clause [tildeP takeApartP]
                        (normalB [| execStateT $(doWithArgs 'ffiMember) []
                                    >>= makeStructType . reverse |])
                        []
                ]
            ]
    
        argDecl <- instanceD (cxt []) (conT ''ObjCArgument `appT`
                                        conT name `appT` conT name)
            [
                valD (varP 'exportArgument) (normalB [| return |]) [],
                valD (varP 'importArgument) (normalB [| return |]) [],
                funD 'objCTypeString [
                    clause [tildeP takeApartP]
                        (normalB [| "{" ++ structTag ++ "=" ++ 
                                    concat $(mapArgs 'objCTypeString) ++
                                    "}" |])
                        []
                ]
            ]
        
        return [dataDecl, storableDecl, ffiDecl, argDecl]
