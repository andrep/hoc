{-# LANGUAGE ForeignFunctionInterface, RankNTypes #-}
module TestFFI where

import HOC.FFICallInterface
import HOC.Invocation


import Foreign
import Foreign.C
import Data.Int
import Test.HUnit

import HOC.Arguments
import HOC.StdArgumentTypes

foreign import ccall "ffi.h ffi_call"
    ffi_call :: FFICif -> FunPtr a -> Ptr b -> Ptr (Ptr ()) -> IO Int
    

fficallDirectly cif fp args
    = allocaRetval $ \ret -> do
        ffi_call cif fp ret args
        peekRetval ret

type Invoker = forall a b c d. 
    (ObjCArgument a a, ObjCArgument b b) =>
    FFICif -> FunPtr (a -> b) -> Ptr (Ptr ()) -> IO b

testArgAndResult :: (Num a, Num b, ObjCArgument a a, ObjCArgument b b)
                 => Invoker -> FunPtr (a -> b) -> IO ()


testArgAndResult invoker fp
    = do
        fixTypes fp theArg theResult

        argType <- makeFFIType theArg
        retType <- makeFFIType theResult
        cif <- ffiPrepCif retType [argType]
        x <- with theArg $ \arg -> withArray [arg] $ \args -> invoker cif fp (castPtr args)
        x @?= theResult
        return ()
    where
        theArg = 23
        theResult = 19
        
        fixTypes :: FunPtr (a -> b) -> a -> b -> IO ()
        fixTypes f a b = return ()



subtractFrom42 x = 42 - x

foreign export ccall "funIntToInt" subtractFrom42 :: CInt -> CInt
foreign import ccall "&funIntToInt" funIntToInt :: FunPtr (CInt -> CInt)

foreign export ccall "funCharToChar" subtractFrom42 :: CChar -> CChar
foreign import ccall "&funCharToChar" funCharToChar :: FunPtr (CChar -> CChar)

foreign export ccall "funFloatToFloat" subtractFrom42 :: CFloat -> CFloat
foreign import ccall "&funFloatToFloat" funFloatToFloat :: FunPtr (CFloat -> CFloat)

foreign export ccall "funDoubleToDouble" subtractFrom42 :: CDouble -> CDouble
foreign import ccall "&funDoubleToDouble" funDoubleToDouble :: FunPtr (CDouble -> CDouble)

foreign export ccall "funLLongToLLong" subtractFrom42 :: CLLong -> CLLong
foreign import ccall "&funLLongToLLong" funLLongToLLong :: FunPtr (CLLong -> CLLong)

testArgsAndResults :: Invoker -> Test


testArgsAndResults invoker
    = test [
        testArgAndResult invoker funIntToInt,
        testArgAndResult invoker funCharToChar,
        testArgAndResult invoker funLLongToLLong,
        testArgAndResult invoker funFloatToFloat,
        testArgAndResult invoker funDoubleToDouble
    ]

tests = "TestFFI" ~: test [
        "peekRetval" ~: test $ with (42 :: Int) $ \p -> do
                ret <- peekRetval (castPtr p :: Ptr CChar)
                ret @?= (42 :: CChar)
            ,
        "Plain FFI" ~: testArgsAndResults fficallDirectly,
        "callWithRetval" ~: testArgsAndResults callWithRetval
    ]
