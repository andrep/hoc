module HOC.SelectorNameMangling where

import Data.List(intersperse)
import Data.Char(isUpper, toUpper, toLower)

-- addObject:forKey: -> addObjectForKey
-- close: -> close

mangleSelectorName = concat .
                     lowercaseFirst .
                     words . uncolon
    where
        uncolon = map f where f ':' = ' ' ; f x = x
        lowercaseFirst (x:xs) = forceLowercase x : map forceUppercase xs
        forceUppercase (x:xs) = toUpper x : xs
        forceLowercase xs = map toLower (takeWhile isUpper xs) ++ dropWhile isUpper xs

{-
-- addObject:forKey: -> addObject_forKey
-- close: -> close

mangleSelectorName = concat . intersperse "_" .
                     (\(x:xs) -> forceLowercase x : xs) .
                     words . uncolon
    where
        uncolon = map f where f ':' = ' ' ; f x = x
        -- forceUppercase (x:xs) = toUpper x : xs
        forceLowercase xs = map toLower (takeWhile isUpper xs) ++ dropWhile isUpper xs

-}

-- addObject:forKey: -> addObject_forKey_
-- close: -> close_

mangleSelectorNameWithUnderscores = forceLowercase . uncolon
    where
        uncolon = map f where f ':' = '_' ; f x = x
        forceLowercase xs = map toLower (takeWhile isUpper xs) ++ dropWhile isUpper xs
