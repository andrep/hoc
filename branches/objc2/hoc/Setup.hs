import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import System.Cmd( system )
import System.Exit( ExitCode(..) )
import System.Environment( getEnv )
import System.FilePath
import System.IO
import System.Process
import qualified System.Info

main = defaultMainWithHooks $ simpleUserHooks {
        confHook = customConfig,
        preBuild = customPreBuild
    }
    
backquote :: String -> IO String
backquote cmd = do
    (inp,out,err,pid) <- runInteractiveCommand cmd
    hClose inp
    text <- hGetContents out
    waitForProcess pid
    hClose err
    return $ init text ++ let c = last text in if c == '\n' then [] else [c]

gnustepPaths :: IO (String, String, String)
gnustepPaths = do
    libgcc <- backquote "gcc --print-libgcc-file-name"
    headersAndLibraries <- backquote
            "opentool /bin/sh -c \
            \'. $GNUSTEP_MAKEFILES/filesystem.sh \
            \; echo $GNUSTEP_SYSTEM_HEADERS ; echo $GNUSTEP_SYSTEM_LIBRARIES'"

    let gcclibdir =  takeDirectory libgcc

    let system_headers : system_libs : _ = lines headersAndLibraries    
    -- sysroot <- getEnv "GNUSTEP_SYSTEM_ROOT"
    -- let system_headers = gnustepsysroot </> "Library/Headers"
    --    system_libs = gnustepsysroot </> "Library/Libraries"
    return (gcclibdir, system_libs, system_headers)

customConfig :: (Either GenericPackageDescription PackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfig pdbi cf = do
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            (gcclibdir, system_libs, system_headers) <- gnustepPaths
            writeFile "HOC.buildinfo" $ "extra-lib-dirs: " ++ gcclibdir ++ ", " ++ system_libs ++ "\n"

    return lbi

customPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
customPreBuild args buildFlags = do
    putStrLn "Compiling HOC_cbits..."
    system "mkdir -p dist/build/"
    
    (cflags, paths, extralibs) <- 
        if System.Info.os == "darwin"
            then do
                return ("-I/usr/include/ffi -DMACOSX", [], ["-framework Foundation"])
            else do
                (gcclibdir, system_libs, system_headers) <- gnustepPaths
                ffi_cflags <- backquote "pkg-config libffi --cflags"
                return ("-I" ++ system_headers ++ " -DGNUSTEP" ++ " " ++ ffi_cflags,
                        ["-L" ++ gcclibdir, "-L" ++ system_libs],
                        ["-lgnustep-base"])

    exitCode <- system $ "gcc -r -nostdlib -I`ghc --print-libdir`/include "
                    ++ cflags ++ " HOC_cbits/*.m -o dist/build/HOC_cbits.o"

    case exitCode of
        ExitSuccess -> return ()
        _ -> fail "Failed in C compilation."
    
    -- system "cp dist/build/HOC_cbits.o dist/build/HOC_cbits.dyn_o"
    system "cp dist/build/HOC_cbits.o dist/build/hoc-test/hoc-test-tmp/"
    
    let buildInfo = emptyBuildInfo {
            options = [ (GHC, ["dist/build/HOC_cbits.o" ]
                              ++ paths ++
                              ["-lobjc",
                               "-lffi"]
                              ++ extralibs) ],
            cSources = ["HOC_cbits.o"]
        }
        buildInfo2 = emptyBuildInfo {
            options = [ (GHC, ["dist/build/hoc-test/hoc-test-tmp/HOC_cbits.o" ]
                              ++ paths ++
                              ["-lobjc",
                               "-lffi"]
                              ++ extralibs) ]{-,
            cSources = ["HOC_cbits.o"]-}
        }
        
    return (Just buildInfo, [("hoc-test", buildInfo2)])

