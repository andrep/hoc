-- above NSGeometry
-- CUT HERE
-- below NSGeometry
-- CUT HERE
-- above NSGeometry.Forward
--X NSPoint(..)
--X NSSize(..)
--X NSRect(..)
--X nsZeroPoint
--X nsZeroSize
--X nsZeroRect
--X nsMaxX
--X nsMaxY
--X nsMidX
--X nsMidY
--X nsMinX
--X nsMinY
--X nsWidth
--X nsHeight


import HOC.FFICallInterface
import HOC.Arguments
import Foreign
-- CUT HERE
-- below NSGeometry.Forward

data NSPoint = NSPoint Float Float  deriving(Read, Show, Eq)
data NSSize = NSSize Float Float    deriving(Read, Show, Eq)
data NSRect = NSRect NSPoint NSSize deriving(Read, Show, Eq)

nsZeroPoint = NSPoint 0 0
nsZeroSize = NSSize 0 0
nsZeroRect = NSRect nsZeroPoint nsZeroSize

nsMaxX (NSRect (NSPoint x y) (NSSize w h)) = x + w
nsMaxY (NSRect (NSPoint x y) (NSSize w h)) = y + h
nsMidX (NSRect (NSPoint x y) (NSSize w h)) = x + w / 2.0
nsMidY (NSRect (NSPoint x y) (NSSize w h)) = y + h / 2.0
nsMinX (NSRect (NSPoint x y) (NSSize w h)) = x
nsMinY (NSRect (NSPoint x y) (NSSize w h)) = y
nsWidth (NSRect (NSPoint x y) (NSSize w h)) = w
nsHeight (NSRect (NSPoint x y) (NSSize w h)) = h

instance Storable NSPoint where
    alignment _ = alignment (undefined :: Float)
    sizeOf _ = 2 * sizeOf (undefined :: Float)
    peek p = do x <- peekElemOff (castPtr p) 0
                y <- peekElemOff (castPtr p) 1
                return (NSPoint x y)
    poke p (NSPoint x y) = do pokeElemOff (castPtr p) 0 x
                              pokeElemOff (castPtr p) 1 y



instance Storable NSSize where
    alignment _ = alignment (undefined :: Float)
    sizeOf _ = 2 * sizeOf (undefined :: Float)
    peek p = do w <- peekElemOff (castPtr p) 0
                h <- peekElemOff (castPtr p) 1
                return (NSSize w h)
    poke p (NSSize w h) = do pokeElemOff (castPtr p) 0 w
                             pokeElemOff (castPtr p) 1 h

instance Storable NSRect where
    alignment _ = alignment (undefined :: NSPoint)
    sizeOf _ = 2 * sizeOf (undefined :: NSPoint)
    peek p = do o <- peekElemOff (castPtr p) 0
                s <- peekElemOff (castPtr p) 1
                return (NSRect o s)
    poke p (NSRect o s) = do pokeElemOff (castPtr p) 0 o
                             pokeElemOff (castPtr p) 1 s

instance FFITypeable NSPoint where
    makeFFIType _ = do float <- makeFFIType (undefined :: Float)
                       makeStructType [float, float]
    isStructType _ = True

instance FFITypeable NSSize where
    makeFFIType _ = do float <- makeFFIType (undefined :: Float)
                       makeStructType [float, float]
    isStructType _ = True

instance FFITypeable NSRect where
    makeFFIType _ = do point <- makeFFIType (undefined :: NSPoint)
                       size <- makeFFIType (undefined :: NSSize)
                       makeStructType [point, size]
    isStructType _ = True


$(declareStorableObjCArgument [t| NSPoint |] "{_NSPoint=ff}")
$(declareStorableObjCArgument [t| NSSize |] "{_NSSize=ff}")
$(declareStorableObjCArgument [t| NSRect |] "{_NSRect={_NSPoint=ff}{_NSSize=ff}}")
