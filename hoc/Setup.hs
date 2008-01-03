import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import System.Cmd( system )
import System.Exit( ExitCode(..) )

main = defaultMainWithHooks $ simpleUserHooks {
        preBuild = customPreBuild
       
        -- ,buildHook = \pd lbi uh bf -> print pd
    }

customPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
customPreBuild args buildFlags = do
    putStrLn "Compiling HOC_cbits..."
    system "mkdir dist/build/"
    exitCode <- system "gcc -r -nostdlib -I`ghc --print-libdir`/include -DMACOSX HOC_cbits/*.m -o dist/build/HOC_cbits.o"
    
    case exitCode of
        ExitSuccess -> return ()
        _ -> fail "Failed in C compilation."
    
    let buildInfo = emptyBuildInfo {
            options = [ (GHC, ["dist/build/HOC_cbits.o",
                               "-lobjc",
                               "-lffi",
                               "-framework Foundation"]) ],
            cSources = ["HOC_cbits.o"]
        }
        
    return (Just buildInfo, [])