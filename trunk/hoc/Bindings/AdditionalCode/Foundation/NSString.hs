--X haskellString
--X stringWithHaskellString
--X nsString
--X toNSString
--X fromNSString

import Foreign.C.String
import Foundation.NSObject  ( alloc )
import System.IO.Unsafe     ( unsafePerformIO )
-- CUT HERE

haskellString :: NSString a -> IO String
nsString :: String -> IO (NSString ())
stringWithHaskellString :: String -> NSStringClass a -> IO (NSString a)

    -- export and reimport as the other type
haskellString nsstr = withExportedArgument nsstr importArgument
nsString nsstr = withExportedArgument nsstr importArgument

    -- and for completeness...
stringWithHaskellString str cls = do
    withCString str (\cstr -> cls # alloc >>= initWithUTF8String cstr)

-- PURE variants:

-- NSString is immutable, so we may do somethings outside the IO monad.

-- If a NSString has been created using toNSString, it should never be compared
-- using (==), because that yields undefined results.
 
toNSString :: String -> NSString ()
toNSString = unsafePerformIO . nsString

-- fromNSString does not accept a subclass of NSString as an argument,
-- because subclasses can be mutable.
-- You should always make sure that you're not in fact passing a (casted)
-- NSMutableString.

fromNSString :: NSString () -> String
fromNSString = unsafePerformIO . haskellString

-- CUT HERE
import Foreign.C.Types
--X NSStringEncoding
-- CUT HERE
type NSStringEncoding = CUInt
