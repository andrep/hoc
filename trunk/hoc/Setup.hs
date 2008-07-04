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

gnustepPaths :: IO (String, String)
gnustepPaths = do
    (inp,out,err,pid) <- runInteractiveCommand "gcc --print-libgcc-file-name"
    hClose inp
    libgcc <- hGetContents out
    waitForProcess pid
    hClose err
    let gcclibdir =  takeDirectory libgcc
    sysroot <- getEnv "GNUSTEP_SYSTEM_ROOT"

    return (gcclibdir, sysroot)

customConfig :: (Either GenericPackageDescription PackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfig pdbi cf = do
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            (gcclibdir, gnustepsysroot) <- gnustepPaths
            writeFile "HOC.buildinfo" $ "extra-lib-dirs: " ++ gcclibdir ++ ", " ++ gnustepsysroot </> "Library/Headers" ++ "\n"

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
                (gcclibdir, sysroot) <- gnustepPaths
                return ("-I$GNUSTEP_SYSTEM_ROOT/Library/Headers -DGNUSTEP",
                        ["-L" ++ gcclibdir, "-L" ++ sysroot </> "Library/Libraries"],
                        ["-lgnustep-base"])
    
    exitCode <- system $ "gcc -r -nostdlib -I`ghc --print-libdir`/include "
                    ++ cflags ++ " HOC_cbits/*.m -o dist/build/HOC_cbits.o"

    case exitCode of
        ExitSuccess -> return ()
        _ -> fail "Failed in C compilation."
    
    -- system "cp dist/build/HOC_cbits.o dist/build/HOC_cbits.dyn_o"
    
    let buildInfo = emptyBuildInfo {
            options = [ (GHC, ["dist/build/HOC_cbits.o" ]
                              ++ paths ++
                              ["-lobjc",
                               "-lffi"]
                              ++ extralibs) ],
            cSources = ["HOC_cbits.o"]
        }
        
    return (Just buildInfo, [])

