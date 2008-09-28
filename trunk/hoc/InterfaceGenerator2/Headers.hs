module Headers( ModuleName,
                HeaderInfo(..),
                headersIn,
                headersForFramework,
                loadHeaders ) where

import Parser(header)
import SyntaxTree(Declaration)

import Control.Exception(evaluate)
import Control.Monad(when)
import Data.Char(isAlphaNum, toUpper)
import Data.List(isPrefixOf,isSuffixOf,partition)
import Data.Maybe(mapMaybe)
import System.Directory(getDirectoryContents)
import System.Info(os)
import Text.Parsec( runParserT )
import Messages( runMessages )
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as BS
import Progress
import Preprocessor
import System.FilePath

type ModuleName = ByteString
data HeaderInfo = HeaderInfo ModuleName [ModuleName] [Declaration]
    deriving(Show)

findImports = mapMaybe checkImport . lines
    where
        checkImport line
            | ("#import" `isPrefixOf` line) || ("#include" `isPrefixOf` line) =
                Just $ init $ tail $ dropWhile (\c -> c /= '"' && c /= '<') line
            | otherwise = Nothing

headersIn dirName prefix = do
    files <- getDirectoryContents dirName
    return [ (fn, dirName </> fn, haskellizeModuleName $
                                 prefix ++ "." ++ takeWhile (/= '.') fn)
           | fn <- files, ".h" `isSuffixOf` fn {- , fn /= (prefix ++ ".h") -} ]

headersForFramework prefix framework =
    if System.Info.os == "darwin"
        then headersIn (prefix </> "System/Library/Frameworks" </> (framework ++ ".framework") </> "Headers") framework
        else headersIn ("/usr/lib/GNUstep/System/Library/Headers/" ++ framework ++ "/") framework

translateObjCImport imp = haskellizeModuleName $
                          map slashToDot $ takeWhile (/= '.') $ imp
    where
        slashToDot '/' = '.'
        slashToDot c = c

loadHeaders (dumpPreprocessed, dumpParsed) progress headers = 
    mapM (\(headerFileName, headerPathName, moduleName) -> do
                -- putStrLn $ "Parsing " ++ headerFileName
                contents <- readFile $ headerPathName
                evaluate (length contents)
                let imports = findImports contents
                    preprocessed = preprocess headerFileName contents
                when dumpPreprocessed $ writeFile ("preprocessed-" ++ headerFileName) $ preprocessed
                
                let (parseResult, parseMessages) = runMessages (runParserT header () headerFileName preprocessed)
                mapM_ print parseMessages
                result <- case parseResult of
                    Left err -> error $ show err
                    Right decls -> do
                        when dumpParsed $ writeFile ("parsed-" ++ headerFileName) $ unlines $ map show decls
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
          firstUpper (x:xs) = toUpper x : upperAfterDot xs
          upperAfterDot ('.':xs) = '.' : firstUpper xs
          upperAfterDot (x:xs) = x : upperAfterDot xs
          upperAfterDot [] = []

          translateChar c | isAlphaNum c || c `elem` "/." = [c]
                          | otherwise = []

