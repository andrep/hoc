module HOC.Exception where

import HOC.Base
import Foreign

exceptionObjCToHaskell :: Ptr ObjCObject -> IO a
exceptionHaskellToObjC :: IO a -> IO (Ptr ObjCObject)
