module Files(
        additionalCodePath,
        outputPath,
        writeFileIfChanged,
        readFileOrEmpty,
        createDirectoryIfNecessary,
        createOutputDirectories
    ) where

import Control.Exception(evaluate)
import Control.Monad(when, unless)
import System.Directory(doesDirectoryExist,
                        doesFileExist,
                        createDirectory)

outputDir = "ifgen-output"
outputPath f = outputDir ++ "/" ++ f

additionalCodePath f = "AdditionalCode/" ++ f

writeFileIfChanged fn text = do
    exists <- doesFileExist fn
    if exists
        then do
            old <- readFile fn
            evaluate $ length old
            when (old /= text) $
                writeFile fn text
        else writeFile fn text

createDirectoryIfNecessary dir = do
    exists <- doesDirectoryExist dir
    unless exists $ createDirectory dir

createOutputDirectories frameworks = do
    createDirectoryIfNecessary outputDir
    mapM_ createDirectoryIfNecessary (map outputPath frameworks)

readFileOrEmpty fn = do
    exists <- doesFileExist fn
    if exists
        then do
            contents <- readFile fn
            return $ Just contents
        else do
            return Nothing
