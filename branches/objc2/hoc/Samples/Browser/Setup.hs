import Distribution.Simple
import System.Cmd

main = defaultMainWithHooks $ simpleUserHooks {
        postBuild = wrapApplication "Browser"
    }

wrapApplication name args buildFlags packageDesc localBuildInfo
    = do
        putStrLn $ "Bundling " ++ name ++ ".app"
        system $ "hoc-wrap dist/build/" ++ name ++ "/" ++ name
        return ()
