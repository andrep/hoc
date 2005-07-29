module HOC.NameCaseChange where

import Data.Char        ( toUpper, toLower, isUpper )

nameToUppercase ('_':ame) = nameToUppercase ame
nameToUppercase ('n':'s':n:ame) | isUpper n = 'N':'S':n:ame
nameToUppercase (n:ame) = toUpper n : ame

nameToLowercase ('N':'S':n:ame) | isUpper n = 'n':'s':n:ame
nameToLowercase (n:ame) = toLower n : ame
