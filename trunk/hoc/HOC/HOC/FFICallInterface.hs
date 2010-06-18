{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module HOC.FFICallInterface(
        FFICif,
        FFIType,
        FFITypeable(..),
        ffiPrepCif,
        makeStructType,
        cifIsStret
    ) where

import Foreign.C.Types
import Foreign
import Data.Int
import Data.Word
import Control.Monad(when)

newtype FFICif = FFICif (Ptr ())
newtype FFIType = FFIType (Ptr ())
    deriving(Storable)

class FFITypeable a where
    makeFFIType :: a{-unused-} -> IO FFIType
    isStructType :: a -> Bool
    
    isStructType _ = False
    
    -- Integral return values of size < sizeof(long) require special treatment
    -- in libffi; they are returned as long. For these types, the following
    -- gives us a chance to replace alloca & peek with a version that undoes
    -- the promotion.
    
    peekRetval :: Storable a => Ptr a -> IO a
    allocaRetval :: Storable a => (Ptr a -> IO b) -> IO b
    
    peekRetval = peek
    allocaRetval = alloca

foreign import ccall "ffi.h &ffi_type_void"       ffi_type_void :: FFIType
foreign import ccall "ffi.h &ffi_type_uint8"      ffi_type_uint8:: FFIType
foreign import ccall "ffi.h &ffi_type_sint8"      ffi_type_sint8:: FFIType
foreign import ccall "ffi.h &ffi_type_uint16"     ffi_type_uint16:: FFIType
foreign import ccall "ffi.h &ffi_type_sint16"     ffi_type_sint16:: FFIType
foreign import ccall "ffi.h &ffi_type_uint32"     ffi_type_uint32:: FFIType
foreign import ccall "ffi.h &ffi_type_sint32"     ffi_type_sint32:: FFIType
foreign import ccall "ffi.h &ffi_type_uint64"     ffi_type_uint64:: FFIType
foreign import ccall "ffi.h &ffi_type_sint64"     ffi_type_sint64:: FFIType
foreign import ccall "ffi.h &ffi_type_float"      ffi_type_float:: FFIType
foreign import ccall "ffi.h &ffi_type_double"     ffi_type_double:: FFIType
foreign import ccall "ffi.h &ffi_type_longdouble" ffi_type_longdouble :: FFIType
foreign import ccall "ffi.h &ffi_type_pointer"    ffi_type_pointer:: FFIType

foreign import ccall "ffi.h ffi_prep_cif"
    ffi_prep_cif :: FFICif -> Int -> Int -> FFIType -> Ptr FFIType -> IO Int

foreign import ccall "FFICallInterface.h allocCif" allocCif :: IO FFICif
foreign import ccall "FFICallInterface.h defaultABI" defaultABI :: Int

ffiPrepCif :: FFIType -> [FFIType] -> IO FFICif
ffiPrepCif ret args = do
    cif <- allocCif
    argArray <- newArray args   -- don't release: pointer gets stored in CIF
    result <- ffi_prep_cif cif defaultABI (length args) ret argArray
    when (result /= 0) $ error $ "libffi error: " ++ show result
    return cif

foreign import ccall unsafe allocStructType :: Ptr FFIType -> IO FFIType

makeStructType :: [FFIType] -> IO FFIType

makeStructType members = do
    members <- newArray0 (FFIType nullPtr) members
    allocStructType members

foreign import ccall unsafe cifIsStret :: FFICif -> IO CInt

promotedPeek p
    = peek (castPtr p :: Ptr CLong) >>= return . fromIntegral

promotedAlloca f = alloca (\intPtr -> f $ castPtr (intPtr :: Ptr CLong))

-- typeable instances
    
instance FFITypeable () where
    makeFFIType _ = return ffi_type_void

instance FFITypeable (Ptr a) where
    makeFFIType _ = return ffi_type_pointer

instance FFITypeable Int8 where
    makeFFIType _ = return ffi_type_sint8
    
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca
    
instance FFITypeable Int16 where
    makeFFIType _ = return ffi_type_sint16

    peekRetval = promotedPeek
    allocaRetval = promotedAlloca

instance FFITypeable Int32 where
    makeFFIType _ = return ffi_type_sint32

    peekRetval = promotedPeek       -- only takes effect on 64-bit
    allocaRetval = promotedAlloca

instance FFITypeable Int64 where
    makeFFIType _ = return ffi_type_sint64

instance FFITypeable Word8 where
    makeFFIType _ = return ffi_type_uint8

    peekRetval = promotedPeek
    allocaRetval = promotedAlloca

instance FFITypeable Word16 where
    makeFFIType _ = return ffi_type_uint16

    peekRetval = promotedPeek
    allocaRetval = promotedAlloca

instance FFITypeable Word32 where
    makeFFIType _ = return ffi_type_uint32

    peekRetval = promotedPeek       -- only takes effect on 64-bit
    allocaRetval = promotedAlloca

instance FFITypeable Word64 where
    makeFFIType _ = return ffi_type_uint64


instance FFITypeable CFloat where
    makeFFIType _ = return ffi_type_float
instance FFITypeable CDouble where
    makeFFIType _ = return ffi_type_double

-- ### FIXME: this should be autoconfigured.
-- The following are correct for Mac OS X
instance FFITypeable Int where
    makeFFIType _ = return ffi_type_sint32

instance FFITypeable Float where
    makeFFIType _ = return ffi_type_float
instance FFITypeable Double where
    makeFFIType _ = return ffi_type_double


instance FFITypeable CChar where
    makeFFIType _ = return ffi_type_sint8
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca
instance FFITypeable CUChar where
    makeFFIType _ = return ffi_type_uint8
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca
instance FFITypeable CSChar where
    makeFFIType _ = return ffi_type_sint8
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca

instance FFITypeable CShort where
    makeFFIType _ = return ffi_type_sint16
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca
instance FFITypeable CUShort where
    makeFFIType _ = return ffi_type_uint16
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca
    
instance FFITypeable CInt where
    makeFFIType _ = return ffi_type_sint32
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca
instance FFITypeable CUInt where
    makeFFIType _ = return ffi_type_uint32
    peekRetval = promotedPeek
    allocaRetval = promotedAlloca

instance FFITypeable CLong where
    makeFFIType _ = return ffi_type_sint32
instance FFITypeable CULong where
    makeFFIType _ = return ffi_type_uint32

instance FFITypeable CLLong where
    makeFFIType _ = return ffi_type_sint64
instance FFITypeable CULLong where
    makeFFIType _ = return ffi_type_uint64
