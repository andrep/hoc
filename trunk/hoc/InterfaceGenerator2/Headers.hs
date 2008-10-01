module Headers( ModuleName,
                HeaderInfo(..),
                headersIn,
                headersForFramework,
                loadHeaders ) where

import Parser(header)
import ParserBase(emptyParseEnvironment)
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
import Data.Graph.Inductive
import Text.Parsec( getState )
import qualified Data.Map as Map

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

loadHeaders (dumpPreprocessed, dumpParsed) progress headers = do
    loaded <- mapM (\(headerFileName, headerPathName, moduleName) -> do
            contents <- readFile $ headerPathName
            evaluate (length contents)
            let imports = findImports contents

            return (headerFileName, BS.pack moduleName, map (BS.pack . translateObjCImport) imports, contents)
        ) headers
        
    let moduleNames = [ n | (_, n, _, _) <- loaded ]
        namesToNums = Map.fromList (zip moduleNames [0..])
        numsToHeaders = Map.fromList (zip [0..] loaded)
        graph :: Gr () ()
        graph = mkUGraph [ 0 .. length loaded - 1 ]
                         [ (to, from) | (_, name, includes, _) <- loaded,
                                        from <- Map.lookup name namesToNums,
                                        include <- includes,
                                        to <- Map.lookup include namesToNums ]
        sorted = map (numsToHeaders Map.!) $ topsort graph
    
        process ( (headerFileName, moduleName, imports, contents) : moreHeaders ) env accum
            = do
                let preprocessed = preprocess headerFileName contents
                when dumpPreprocessed $ writeFile ("preprocessed-" ++ headerFileName) $ preprocessed
                
                let parser = do
                        decls <- header
                        env' <- getState
                        return (decls, env')
                    (parseResult, parseMessages) = 
                        runMessages (runParserT parser env headerFileName preprocessed)
                mapM_ print parseMessages
                case parseResult of
                    Left err -> fail $ show err
                    Right (decls, env') -> do
                        when dumpParsed $ writeFile ("parsed-" ++ headerFileName) $ unlines $ map show decls
                        reportProgress progress nHeaders
                        let result = HeaderInfo moduleName imports decls
                        
                        process moreHeaders env' (result : accum)
        process [] _ accum = return accum

    process sorted emptyParseEnvironment [] >>= return . reverse
    
    where
        nHeaders = length headers

haskellizeModuleName = firstUpper . concatMap translateChar
    where firstUpper [] = []
          firstUpper (x:xs) = toUpper x : upperAfterDot xs
          upperAfterDot ('.':xs) = '.' : firstUpper xs
          upperAfterDot (x:xs) = x : upperAfterDot xs
          upperAfterDot [] = []

          translateChar c | isAlphaNum c || c `elem` "/." = [c]
                          | otherwise = []

