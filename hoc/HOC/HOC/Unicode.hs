--------------------------------------------------------------------------------
--
--  This module has been copied from the HXML Toolbox library:
--    http://www.fh-wedel.de/~si/HXmlToolbox/
--  and is subject to the following terms:
--
--  The MIT License
--
--  Copyright (c) 2002 Uwe Schmidt, Martin Schmidt
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"),
--  to deal in the Software without restriction, including without limitation
--  the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the
--  Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
--  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
--  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
--  OR OTHER DEALINGS IN THE SOFTWARE.
--
--------------------------------------------------------------------------------

--  Modified, May 2004, Graham Klyne:
--
--  - Added generic UTF-to-Unicode conversion function
--  - Return a null Unicode character when an invalid UTF-8 sequence is encountered

--  Modified, March 2006, Wolfgang Thaller;
--
--  - Brutally cut down for inclusion in HOC
--    all we need is Unicode <-> UTF8 conversion.
--  - Use String for unicode and [Word8] for utf-8,
--    because that's The Way It Should Be (according to me).


module HOC.Unicode
    (
     -- * Unicode Type declarations
     UTF8Char,
     UTF8String,

      -- * Unicode and UTF-8 conversions
      utf8ToUnicode,
      unicodeToUtf8
    )
where

import Data.Word( Word8 )

-- ------------------------------------------------------------

-- | Unicode is represented as the Char type
--   Precondition for this is the support of Unicode character range
--   in the compiler (e.g. ghc but not hugs)
type Unicode    = Char

-- | the type for Unicode strings
type UString    = [Unicode]

type UTF8Char   = Word8

type UTF8String = [UTF8Char]

maxCharValue = fromEnum ('\x10FFFF')

-- ------------------------------------------------------------
--
-- Unicode predicates

-- |
-- test for leading multibyte UTF-8 character

isLeadingMultiByteChar  :: UTF8Char -> Bool
isLeadingMultiByteChar  c
    = c >= 0xC0 && c <= 0xFD

-- |
-- test for following multibyte UTF-8 character

isFollowingMultiByteChar        :: UTF8Char -> Bool
isFollowingMultiByteChar        c
    = c >= 0x80 && c < 0xC0

-- |
-- compute the number of following bytes and the mask bits of a leading UTF-8 multibyte char

isNByteChar     :: UTF8Char -> (Int, Int, Int)
isNByteChar c
    | c >= 0xc0 && c <= 0xdf = (1, 0xC0, 0x00000080)
    | c >= 0xe0 && c <= 0xef = (2, 0xE0, 0x00000800)
    | c >= 0xf0 && c <= 0xf7 = (3, 0xF0, 0x00010000)
    | c >= 0xf8 && c <= 0xfb = (4, 0xF8, 0x00200000)
    | c >= 0xfc && c <= 0xfd = (5, 0xFC, 0x04000000)
    | otherwise = (0,fromIntegral c,0)

-- ------------------------------------------------------------

-- |
-- conversion of a UTF-8 encoded string into a sequence of unicode values.
-- precondition: the string is a valid UTF-8 encoded string

utf8ToUnicode           :: UTF8String -> String
utf8ToUnicode (c : cs)
    | c < 0x80
        = toEnum (fromIntegral c) : utf8ToUnicode cs
    | isLeadingMultiByteChar c
      &&
      resAsInt <= maxCharValue
      &&
      l1 == length cs0
        = toEnum resAsInt
          : utf8ToUnicode cs1
    | otherwise
        = toEnum 0 : utf8ToUnicode cs1
          where
          (l1, mask, _min)      = isNByteChar c
          (cs0, cs1)            = splitAt l1 cs

          resAsInt              :: Int
          resAsInt              = utf8ToU (fromIntegral c - mask) cs0

          utf8ToU i []      = i
          utf8ToU i (c1:l)
              | isFollowingMultiByteChar c1
                  = utf8ToU (i * 0x40 + (fromIntegral c1 - 0x80)) l
              | otherwise
                  = 0 -- error ("utf8ToUnicode: illegal UTF-8 multibyte character " ++ show (c : cs0) )

utf8ToUnicode []
    = []

-- ------------------------------------------------------------

-- |
-- conversion from Unicode strings (UString) to UTF8 encoded strings.

unicodeToUtf8           :: UString -> UTF8String
unicodeToUtf8           = concatMap unicodeCharToUtf8

-- |
-- conversion from Char to a UTF8 encoded string.

unicodeCharToUtf8       :: Char -> UTF8String
unicodeCharToUtf8 c
    | i >= 0          && i <= 0x0000007F        -- 1 byte UTF8 (7 bits)
        = [ fromIntegral i ]
    | i >= 0x00000080 && i <= 0x000007FF        -- 2 byte UTF8 (5 + 6 bits)
        = [ fromIntegral (0xC0 + i `div` 0x40)
          , fromIntegral (0x80 + i                  `mod` 0x40)
          ]
    | i >= 0x00000800 && i <= 0x0000FFFF        -- 3 byte UTF8 (4 + 6 + 6 bits)
        = [ fromIntegral (0xE0 +  i `div`   0x1000)
          , fromIntegral (0x80 + (i `div`     0x40) `mod` 0x40)
          , fromIntegral (0x80 +  i                 `mod` 0x40)
          ]
    | i >= 0x00010000 && i <= 0x001FFFFF        -- 4 byte UTF8 (3 + 6 + 6 + 6 bits)
        = [ fromIntegral (0xF0 +  i `div`    0x40000)
          , fromIntegral (0x80 + (i `div`     0x1000) `mod` 0x40)
          , fromIntegral (0x80 + (i `div`       0x40) `mod` 0x40)
          , fromIntegral (0x80 +  i                   `mod` 0x40)
          ]
    | i >= 0x00200000 && i <= 0x03FFFFFF        -- 5 byte UTF8 (2 + 6 + 6 + 6 + 6 bits)
        = [ fromIntegral (0xF8 +  i `div`  0x1000000)
          , fromIntegral (0x80 + (i `div`    0x40000) `mod` 0x40)
          , fromIntegral (0x80 + (i `div`     0x1000) `mod` 0x40)
          , fromIntegral (0x80 + (i `div`       0x40) `mod` 0x40)
          , fromIntegral (0x80 +  i                   `mod` 0x40)
          ]
    | i >= 0x04000000 && i <= 0x7FFFFFFF        -- 6 byte UTF8 (1 + 6 + 6 + 6 + 6 + 6 bits)
        = [ fromIntegral (0xFC +  i `div` 0x40000000)
          , fromIntegral (0x80 + (i `div`  0x1000000) `mod` 0x40)
          , fromIntegral (0x80 + (i `div`    0x40000) `mod` 0x40)
          , fromIntegral (0x80 + (i `div`     0x1000) `mod` 0x40)
          , fromIntegral (0x80 + (i `div`       0x40) `mod` 0x40)
          , fromIntegral (0x80 +  i                   `mod` 0x40)
          ]
    | otherwise                                 -- other values not supported
        = error ("unicodeCharToUtf8: illegal integer argument " ++ show i)
    where
    i = fromEnum c

