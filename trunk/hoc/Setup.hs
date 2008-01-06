import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import System.Cmd( system )
import System.Exit( ExitCode(..) )
import System.Environment( getEnv )
import System.FilePath
import System.IO
import System.Process
import qualified System.Info

main = defaultMainWithHooks $ defaultUserHooks {
        preBuild = customPreBuild
    }

customPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
customPreBuild args buildFlags = do
    putStrLn "Compiling HOC_cbits..."
    system "mkdir -p dist/build/"
    
    (cflags, paths, extralibs) <- 
        if System.Info.os == "darwin"
            then do
                return ("-DMACOSX", [], ["-framework Foundation"])
            else do
                (inp,out,err,pid) <- runInteractiveCommand "gcc --print-libgcc-file-name"
                hClose inp
                libgcc <- hGetContents out
                waitForProcess pid
                hClose err
                let gcclibdir =  takeDirectory libgcc
                sysroot <- getEnv "GNUSTEP_SYSTEM_ROOT"
                return ("-I$GNUSTEP_SYSTEM_ROOT/Library/Headers -DGNUSTEP",
                        ["-L" ++ gcclibdir, "-L" ++ sysroot </> "Library/Libraries"],
                        ["-lgnustep-base"])
    
    exitCode <- system $ "gcc -r -nostdlib -I`ghc --print-libdir`/include "
                    ++ cflags ++ " HOC_cbits/*.m -o dist/build/HOC_cbits.o"

    case exitCode of
        ExitSuccess -> return ()
        _ -> fail "Failed in C compilation."
    
    let buildInfo = emptyBuildInfo {
            options = [ (GHC, ["dist/build/HOC_cbits.o" ]
                              ++ paths ++
                              ["-lobjc",
                               "-lffi"]
                              ++ extralibs) ],
            cSources = ["HOC_cbits.o"]
        }
        
    return (Just buildInfo, [])

