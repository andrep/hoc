module Main where

import Prelude                  hiding ( init )

import Control.Monad            ( when )
import Data.List                ( isSuffixOf )
import System.Console.GetOpt
import System.Environment       ( getArgs, getProgName )
import System.IO                ( hPutStrLn, hClose )
import System.IO.Unsafe         ( unsafePerformIO )
import System.Exit              ( exitWith, ExitCode(..) )
import System.Posix             ( createPipe, dupTo, stdInput, closeFd,
                                  fdToHandle, forkProcess, executeFile,
                                  getProcessStatus )

import HOC
import Foundation.NSFileManager
import Foundation.NSString      hiding ( length )
import Foundation.NSDictionary
import Foundation.NSObject

data Option = OutputApp String
            | Contents String
            | Interpret
            | Interactive
            | Help
            deriving (Eq)

options = [
        Option ['c'] ["contents"]   (ReqArg Contents "<Contents>")
            "Contents folder the .app",
        Option ['o'] ["output-app"] (ReqArg OutputApp "<app-package>")
            "Name of app package to create",
        Option ['i'] ["interpret"] (NoArg Interpret)
            "Run using GHCi",
        Option ['I'] ["interactive"] (NoArg Interactive)
            "Start interactive GHCi session",
        Option ['h'] ["help"] (NoArg Help)
            "Display this message"
    ]

usageHeader prog = unlines $ [
        "Usage:",
        "    " ++ prog ++ " [-o XYZ.app] [-c Contents] executable",
        "    " ++ prog ++ " --interactive [-c Contents] -- [ghci arguments]"
    ]

forceDotApp x | ".app" `isSuffixOf` x = x
              | otherwise = x ++ ".app"

main = do
    prog <- getProgName
    args <- getArgs
    
    let usage = usageInfo (usageHeader prog) options
    
    case getOpt Permute options args of
        (_,_,errs@(_:_)) -> putStrLn (unlines errs) >> putStrLn usage

        (opts,moreArgs,[]) -> do
            let contents = head $ [ s | Contents s <- opts ] ++ ["Contents"]
            if any (`elem` [Interpret, Interactive]) opts
                then do
                    let appName = forceDotApp $ head $ [ s | OutputApp s <- opts ]
                                                ++ ["Interactive Haskell Application"]
                    runApp moreArgs appName contents (any (==Interpret) opts)
                else do
                    let executable = head (moreArgs ++ ["a.out"])
                        appName = forceDotApp $ head $ [ s | OutputApp s <- opts ]
                                                    ++ [executable ++ ".app"]
                    when (length moreArgs > 1) $
                        fail "Too many arguments."
                    wrapApp executable appName contents
            
failOnFalse _ True = return ()
failOnFalse err False = fail err
            
wrapApp' justLink overwrite executable appName contents =
    withAutoreleasePool $ do
        fm <- _NSFileManager # defaultManager

        let nsAppName = toNSString appName
        exists <- fm # fileExistsAtPath nsAppName
        when exists $
            if overwrite
                then fm # removeFileAtPathHandler nsAppName nil
                         >>= failOnFalse "Couldn't remove old .app."
                else fail $ appName ++ " exists; it is in the way."

        let executableInApp = take (length appName - length ".app") appName
             
        fm # createDirectoryAtPathAttributes nsAppName nil 
            >>= failOnFalse "Couldn't create .app."
        fm # copyPathToPathHandler (toNSString contents)
                   {-toPath:-}     (toNSString $ appName ++ "/Contents")
                   {-handler:-}    nil
            >>= failOnFalse "Couldn't copy Contents folder."
            
        let nsMacOSFolder = toNSString (appName ++ "/Contents/MacOS")
        
        exists <- fm # fileExistsAtPath nsMacOSFolder
        when (not exists) $
            fm # createDirectoryAtPathAttributes nsMacOSFolder nil
                >>= failOnFalse "Couldn't create Contents/MacOS"
        
        let copyMethod | justLink  = linkPathToPathHandler
                       | otherwise = copyPathToPathHandler
        
        fm # copyMethod (toNSString executable)
                        (toNSString $ appName ++ "/Contents/MacOS/"
                                              ++ executableInApp)
                        nil
            >>= failOnFalse "Couldn't copy executable."
        
        let nsPListName = toNSString $ appName ++ "/Contents/Info.plist"
        
        infoPList <- _NSMutableDictionary # alloc >>= initWithContentsOfFile nsPListName
        infoPList # setObjectForKey (toNSString executableInApp) (toNSString "CFBundleExecutable")
        infoPList # writeToFileAtomically nsPListName False
            >>= failOnFalse "Couldn't write plist."
        return ()

            
wrapApp executable appName contents =
    wrapApp' False True executable appName contents

-- for HEAD-GHC:
-- forkProcess' action = forkProcess action
-- for STABLE-GHC:
forkProcess' action = do
    mbPid <- forkProcess
    case mbPid of
        Just pid -> return pid
        Nothing -> action >> exitWith ExitSuccess

runApp ghciArgs appName contents runNow = withAutoreleasePool $ do
    fm <- _NSFileManager # defaultManager

    let executableInApp = take (length appName - length ".app") appName
         
    
    let ghcLib = "/usr/local/lib/ghc-6.2"
        ghcExecutable = ghcLib ++ "/ghc-6.2"
    
    wrapApp' True False ghcExecutable appName contents

    let execpath = appName ++ "/Contents/MacOS/" ++ executableInApp
        arguments = ("-B" ++ ghcLib) : "--interactive" : ghciArgs

    pipes <- if runNow
                then fmap Just createPipe
                else return Nothing

    pid <- forkProcess' $ do
        case pipes of
            Just (readEnd, _) ->
                dupTo readEnd stdInput >> return ()
            Nothing ->
                return ()
        executeFile execpath False arguments Nothing
    
    case pipes of
        Just (readEnd, writeEnd) -> do
            closeFd readEnd
            pipeToChild <- fdToHandle writeEnd
            hPutStrLn pipeToChild "main"
            hPutStrLn pipeToChild ":q"
            hClose pipeToChild
        Nothing ->
            return ()
    
    status <- getProcessStatus True False pid
    print status
    
    fm # removeFileAtPathHandler (toNSString appName) nil
        >>= failOnFalse "Couldn't remove .app."
    
    return ()
