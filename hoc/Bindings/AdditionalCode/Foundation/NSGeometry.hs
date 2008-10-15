import Prelude
-- CUT HERE

--X nsMaxX
--X nsMaxY
--X nsMidX
--X nsMidY
--X nsMinX
--X nsMinY
--X nsWidth
--X nsHeight

nsMaxX (NSRect (NSPoint x y) (NSSize w h)) = x + w
nsMaxY (NSRect (NSPoint x y) (NSSize w h)) = y + h
nsMidX (NSRect (NSPoint x y) (NSSize w h)) = x + w / 2.0
nsMidY (NSRect (NSPoint x y) (NSSize w h)) = y + h / 2.0
nsMinX (NSRect (NSPoint x y) (NSSize w h)) = x
nsMinY (NSRect (NSPoint x y) (NSSize w h)) = y
nsWidth (NSRect (NSPoint x y) (NSSize w h)) = w
nsHeight (NSRect (NSPoint x y) (NSSize w h)) = h
