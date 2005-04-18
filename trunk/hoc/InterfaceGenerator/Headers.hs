module Headers where

import Parser(header)
import SyntaxTree(Declaration)

import Control.Exception(evaluate)
import Data.Char(isAlphaNum, toUpper)
import Data.List(isPrefixOf,isSuffixOf,partition)
import Data.Maybe(mapMaybe)
import System.Directory(getDirectoryContents)
import System.Info(os)
import Text.ParserCombinators.Parsec(parse)

type ModuleName = String
data HeaderInfo = HeaderInfo ModuleName [ModuleName] [Declaration]
    deriving(Show)

stripPreprocessor = unlines . stripPP . lines
    where
		stripPP (x@('#':_) : xs) = dropPreprocessorLine x xs
		stripPP (x : xs) = x : stripPP xs
		stripPP [] = []
		dropPreprocessorLine x xs
			| last x == '\\' = "" : dropPreprocessorLine (head xs) (tail xs)
			| otherwise = "" : stripPP xs

findImports = mapMaybe checkImport . lines
    where
        checkImport line
            | ("#import" `isPrefixOf` line) || ("#include" `isPrefixOf` line) =
                Just $ init $ tail $ dropWhile (\c -> c /= '"' && c /= '<') line
            | otherwise = Nothing

headersIn dirName prefix = do
    files <- getDirectoryContents dirName
    return [ (fn, dirName ++ fn, haskellizeModuleName $
                                 prefix ++ "." ++ takeWhile (/= '.') fn)
           | fn <- files, ".h" `isSuffixOf` fn {- , fn /= (prefix ++ ".h") -} ]

headersForFramework framework =
    if System.Info.os == "darwin"
        then headersIn ("/System/Library/Frameworks/" ++ framework ++ ".framework/Headers/") framework
        else headersIn ("/usr/lib/GNUstep/System/Library/Headers/" ++ framework ++ "/") framework

translateObjCImport imp = haskellizeModuleName $
                          map slashToDot $ takeWhile (/= '.') $ imp
    where
        slashToDot '/' = '.'
        slashToDot c = c

loadHeaders headers = 
    mapM (\(headerFileName, headerPathName, moduleName) -> do
                putStrLn $ "Parsing " ++ headerFileName
                contents <- readFile $ headerPathName
                evaluate (length contents)
                let imports = findImports contents
                    preprocessed = stripPreprocessor contents
                case parse header headerFileName preprocessed of
                    Left err -> error $ show err
                    Right decls ->
                        return $ HeaderInfo moduleName (map translateObjCImport imports) decls
            ) headers

orderModules :: [HeaderInfo] -> [HeaderInfo]

orderModules [] = []
orderModules mods = if null ok
                    then (head notOK) : orderModules (tail notOK)
                    else ok ++ orderModules notOK
    where
        (notOK, ok) = partition (\(HeaderInfo name imports decls) ->
                                  any (`elem` names) imports) mods
        names = map (\(HeaderInfo name imports decls) -> name) mods
        -- names | any ("Foundation." `isPrefixOf`) names' = "Foundation.Foundation" : names'
        --     | otherwise = names'


haskellizeModuleName = firstUpper . concatMap translateChar
    where firstUpper [] = []
          firstUpper (x:xs) = toUpper x : xs
          translateChar c | isAlphaNum c || c `elem` "/." = [c]
                          | otherwise = []
