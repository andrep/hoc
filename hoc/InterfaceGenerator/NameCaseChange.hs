module NameCaseChange where

import Data.Char        ( toUpper, toLower )

nameToUppercase ('_':ame) = ame
nameToUppercase (n:ame) = toUpper n : ame
nameToLowercase (n:ame) = toLower n : ame

