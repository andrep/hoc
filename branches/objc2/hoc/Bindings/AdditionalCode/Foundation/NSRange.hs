--X nsMaxRange
--X nsLocationInRange

{-import Foreign
import Foreign.C.Types-}
import Prelude
-- CUT HERE
-- below NSRange.Forward

-- data NSRange = NSRange CUInt CUInt  deriving(Read, Show, Eq)

nsMaxRange (NSRange loc len) = loc + len
nsLocationInRange x (NSRange loc len) = x >= loc && x < loc+len

{-
instance Storable NSRange where
    alignment _ = alignment (undefined :: CUInt)
    sizeOf _ = 2 * sizeOf (undefined :: CUInt)
    peek p = do loc <- peekElemOff (castPtr p) 0
                len <- peekElemOff (castPtr p) 1
                return (NSRange loc len)
    poke p (NSRange loc len) = do pokeElemOff (castPtr p) 0 loc
                                  pokeElemOff (castPtr p) 1 len


instance FFITypeable NSRange where
    makeFFIType _ = do cuint <- makeFFIType (undefined :: CUInt)
                       makeStructType [cuint, cuint]
    isStructType _ = True

$(declareStorableObjCArgument [t| NSRange |] "{_NSRange=II}")
-}