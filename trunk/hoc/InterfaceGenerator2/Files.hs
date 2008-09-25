module Files(
        writeFileIfChanged,
        createDirectoryIfNecessary,
        createParentDirectoriesIfNecessary
    ) where

import Control.Exception(evaluate)
import Control.Monad(when, unless)
import System.Directory(doesDirectoryExist,
                        doesFileExist,
                        createDirectory)

writeFileIfChanged :: FilePath -> String -> IO ()
writeFileIfChanged fn text = do
    exists <- doesFileExist fn
    if exists
        then do
            old <- readFile fn
            evaluate $ length old
            when (old /= text) $
                writeFile fn text
        else writeFile fn text

createDirectoryIfNecessary :: FilePath -> IO ()
createDirectoryIfNecessary dir = do
    exists <- doesDirectoryExist dir
    unless exists $ createDirectory dir

createParentDirectoriesIfNecessary :: FilePath -> IO ()
createParentDirectoriesIfNecessary f
    = work (dropWhile (/= '/') $ reverse f)
    where
        work "" = return ()
        work ('/' : fr) = work fr
        work fr = do
            work $ dropWhile (/='/') fr
            createDirectoryIfNecessary (reverse fr)
