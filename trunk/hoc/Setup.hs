import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Build
import Distribution.Simple.Setup
import Distribution.Simple.PreProcess
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import System.Cmd( system )
import System.Exit( ExitCode(..) )
import System.Environment( getEnv )
import System.FilePath
import System.IO
import System.Process
import qualified System.Info

main = defaultMainWithHooks $ defaultUserHooks {
        confHook = customConfig,
        buildHook = customBuild
    }

-- You probably don't need to change this, but if you do beware that
-- it will not be sanitized for the shell.
cbitsObjectFile = "dist/build/HOC_cbits.o"

needsCBitsWhileBuilding :: Executable -> Bool
needsCBitsWhileBuilding e
        | exeName e == "hoc-test"       = True
        | otherwise                     = False

objc2_flagName = FlagName "objc2"

setObjC2Flag :: ConfigFlags -> IO ConfigFlags
setObjC2Flag cf
    -- if the flag is set on the command line, do nothing
    | lookup (objc2_flagName) (configConfigurationsFlags cf) /= Nothing
    = return cf
    
    -- if we're not on darwin, assume false
    | System.Info.os /= "darwin"
    = return $ addFlag objc2_flagName False cf
    
    -- otherwise make an educated guess
    | otherwise
    = do
        value <- objC2Available
        return $ addFlag objc2_flagName value cf
    
    where addFlag flag value cf = cf { configConfigurationsFlags = 
                (flag,value) : configConfigurationsFlags cf }

objC2Available :: IO Bool
objC2Available 
    | System.Info.os /= "darwin"    = return False
    | otherwise = do
        result <- system "grep -qR /usr/include/objc -e objc_allocateClassPair"
        return (result == ExitSuccess)

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
    cf <- setObjC2Flag cf
    
    lbi <- configure pdbi cf
    if System.Info.os == "darwin"
        then return()
        else do
            (gcclibdir, system_libs, system_headers) <- gnustepPaths
            writeFile "HOC.buildinfo" $ unlines [
                "extra-lib-dirs: " ++ gcclibdir ++ ", " ++ system_libs,
                "include-dirs: " ++ system_headers ]

    return lbi

customBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuild pd lbi hooks buildFlags = do
    let Just libInfo = library pd
    extraFlags <- buildCBits (libBuildInfo libInfo)
    
    -- add compiler flags required by C parts of HOC;
    -- 
    -- HACK #1:
    -- This passes the HOC_cbits.o object file on the
    -- as a compiler flag so that template haskell can link
    -- compile-time code.
    
    let hooked_pd = pd 
                { library = Just $ libInfo
                        { libBuildInfo = addCompilerFlags extraFlags
                                                (libBuildInfo libInfo)
                        }
                , executables = alterExecutable needsCBitsWhileBuilding
                        (\exe -> exe {buildInfo = addCompilerFlags extraFlags (buildInfo exe)})
                        (executables pd)
                }
    
    -- HACK #2:
    -- HOC_cbits.o (built by buildCBits below) is specified in c-sources
    -- for the library. Cabal reacts to this by invoking ghc -c HOC_cbits.o,
    -- which ghc doesn't like. So, instead of calling ghc directly, we call
    -- a short auto-generated shell script that does nothing in this case,
    -- and calls the real ghc in all other cases.
    -- After having "compiled" HOC_cbits.o in this way, Cabal will link
    -- HOC_cbits.o as part of the library, which is what we want.
    
    --print ((\(ProgramConfiguration a b) -> (a,b)) $ withPrograms lbi)
    let -- Just pr = lookupKnownProgram "ghc" (withPrograms lbi)
        pr = simpleProgram "ghc"
        Just conf = lookupProgram pr (withPrograms lbi)
    
        ghcLocation = programLocation conf
        
    let fakeGHC = "./dist/build/fake-ghc.sh"
    
    writeFile fakeGHC $ unlines [
            "#!/bin/sh -e",
            "case \"$*\" in",
            "    *HOC_cbits.o) true;;",
            "    *) \"" ++ locationPath ghcLocation ++ "\" \"$@\";;",
            "esac"
        ]
    system $ "chmod +x " ++ fakeGHC
    
    let conf' = conf { programLocation = UserSpecified fakeGHC }
        
        progs' = updateProgram conf' (withPrograms lbi)
    
    let lbi' = lbi {
                    withPrograms = progs'
                }
    
    -- call the default with our modified package description and
    -- local build info
    build hooked_pd lbi' buildFlags knownSuffixHandlers

-- |Build HOC_cbits.o using the flags specified in the configuration
-- stage, and return a list of flags to add to support usage of 
-- template-haskell while compiling (for both the library and the
-- hoc-test executable)
buildCBits :: BuildInfo -> IO [(CompilerFlavor, [String])]
buildCBits buildInfo = do
    putStrLn "Compiling HOC_cbits..."

    system ("mkdir -p " ++ takeDirectory cbitsObjectFile)
    
    let cflags = cppOptions buildInfo ++ ccOptions buildInfo
                ++ ["-I" ++ dir | dir <- includeDirs buildInfo] 
        extraGHCflags = [cbitsObjectFile]
                ++ ["-L" ++ lib | lib <- extraLibDirs buildInfo]
                ++ ["-l" ++ lib | lib <- extraLibs buildInfo]
                ++ ["-framework " ++ fw | fw <- frameworks buildInfo]
    
    let cmd = "gcc -r -nostdlib -I`ghc --print-libdir`/include "
                    ++ unwords cflags
                    ++ " HOC_cbits/*.m -o " ++ cbitsObjectFile
    
    exitCode <- system cmd

    case exitCode of
        ExitSuccess -> return ()
        _ -> fail "Failed in C compilation."
    
    return [(GHC, extraGHCflags)]

-- TODO: check whether it's OK for the options field to have multiple
-- entries for the same "compiler flavor"
addCompilerFlags :: [(CompilerFlavor,[String])] -> BuildInfo -> BuildInfo
addCompilerFlags flags buildInfo = buildInfo {
        options = flags ++ options buildInfo
    }

alterExecutable :: (Executable -> Bool) -> (Executable -> Executable) 
        -> [Executable] -> [Executable]
alterExecutable p f exes = [if p exe then f exe else exe | exe <- exes]
