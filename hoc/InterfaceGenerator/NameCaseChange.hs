module NameCaseChange where

import Data.Char        ( toUpper, toLower )

nameToUppercase ('_':ame) = nameToUppercase ame
nameToUppercase (n:ame) = toUpper n : ame
nameToLowercase (n:ame) = toLower n : ame

