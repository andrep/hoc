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
    putStrLn "preBuild"
    -- print buildFlags
    -- print $ case parseHookedBuildInfo "ghc-options: H.o" of ParseOk _ x -> x
    --let (Just y, []) = case parseHookedBuildInfo "ghc-options: H.o" of ParseOk _ x -> x
    
    -- return (Nothing, [("ATest", y)])
    -- return (Just y, [])
    --return emptyHookedBuildInfo
    
    exitCode <- system "gcc -gstabs+ -r -nostdlib -I`ghc --print-libdir`/include -DMACOSX HOC_cbits/*.m -o dist/build/HOC_cbits.o"
    
    case exitCode of
        ExitSuccess -> return ()
        _ -> fail "Failed in C compilation."
    
    let buildInfo = emptyBuildInfo {
            options = [ (GHC, ["dist/build/HOC_cbits/HOC_cbits.o",
                               "-lobjc",
                               "-framework Foundation"]) ],
            cSources = ["HOC_cbits.o"]
        }
        
    return (Just buildInfo, [])