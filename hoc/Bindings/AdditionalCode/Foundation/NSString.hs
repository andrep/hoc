--X haskellString
--X stringWithHaskellString
--X nsString

import Foreign.C.String
import Foundation.NSObject(alloc)
-- CUT HERE

haskellString :: NSString a -> IO String
stringWithHaskellString :: String -> NSStringClass a -> IO (NSString a)

haskellString nsstr = do
    utf8 <- nsstr # utf8String
    peekCString utf8
    
stringWithHaskellString str cls = do
    withCString str (\cstr -> cls #* alloc >>= initWithUTF8String cstr)

nsString str = _NSString # stringWithHaskellString str
