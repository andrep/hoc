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
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS
import Progress
import Preprocessor

type ModuleName = ByteString
data HeaderInfo = HeaderInfo ModuleName [ModuleName] [Declaration]
    deriving(Show)

stripPreprocessor = unlines . stripPP . lines
    where
        stripPP (('#':'e':'l':'s':'e':_) : xs) = "" : dropElseHack xs
        stripPP (x@('#':_) : xs) = dropPreprocessorLine x xs
        stripPP (x : xs) = x : stripPP xs
        stripPP [] = []
        dropPreprocessorLine x xs
            | last x == '\\' = "" : dropPreprocessorLine (head xs) (tail xs)
            | otherwise = "" : stripPP xs

        dropElseHack (('#':'e':'n':'d':'i':'f':_) : xs) = "" : stripPP xs
        dropElseHack (x : xs) = "" : dropElseHack xs
        dropElseHack [] = []

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
        -- then headersIn ("/System/Library/Frameworks/" ++ framework ++ ".framework/Headers/") framework
        then headersIn ("/Developer/SDKs/MacOSX10.4u.sdk/System/Library/Frameworks/" ++ framework ++ ".framework/Headers/") framework
        else headersIn ("/usr/lib/GNUstep/System/Library/Headers/" ++ framework ++ "/") framework

translateObjCImport imp = haskellizeModuleName $
                          map slashToDot $ takeWhile (/= '.') $ imp
    where
        slashToDot '/' = '.'
        slashToDot c = c

loadHeaders progress headers = 
    mapM (\(headerFileName, headerPathName, moduleName) -> do
                -- putStrLn $ "Parsing " ++ headerFileName
                contents <- readFile $ headerPathName
                evaluate (length contents)
                let imports = findImports contents
                    preprocessed = preprocess headerFileName {- stripPreprocessor -} contents
                result <- case parse header headerFileName preprocessed of
                    Left err -> error $ show err
                    Right decls ->
                        return $ HeaderInfo (BS.pack moduleName)
                                            (map (BS.pack . translateObjCImport) imports) decls
                reportProgress progress nHeaders
                return result
            ) headers
    where
        nHeaders = length headers

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
