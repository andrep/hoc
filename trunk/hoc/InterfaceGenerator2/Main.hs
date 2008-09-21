{-# LANGUAGE CPP #-}
module Main where

import Headers
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import BindingScript
import Data.Char
import Data.Maybe
-- import Data.Generics
import System.IO

import Messages
import Entities
-- import Traversals

import Files
-- import Debug.Trace

import Progress
import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.Console.GetOpt
import Control.Exception

#ifdef BINARY_INTERFACES
import Data.Binary      ( encodeFile, decode )
import BinaryInstances  ()
#endif

import qualified Data.ByteString.Lazy as LBS

import BuildEntities
import ResolveAndZap
import DependenceGraphs
import ShuffleInstances
import DuplicateEntities
import RenameClashingIdentifiers
import Output

textInterfaces = False  -- Overall 3 times faster with binary

{-deepEvaluatePile = mapM_ deepEvaluateEntity . Map.elems . localEntities

evalWithProgress str pile
    = runShowingProgress str $
        \progress -> deepEvaluatePile $ reportProgressForPile progress $ pile
-}

instance Monitorable EntityPile where
    monitor pr = transformLocalEntities (monitor pr)

writeFrameworkModules progress entityPile path
    = do
        let byModule = makeEntityPileLocalMultiIndex eModule $
                       entityPile
        
        let basePathForModule = (path ++) . map (\c -> if c == '.' then '/' else c) . BS.unpack
            hsModulePathForModule m = basePathForModule m ++ ".hs"
            hsBootPathForModule m = basePathForModule m ++ ".hs-boot"
        
        let nModules = Map.size byModule
        
            modGraph = minimizeSourceImports $ makeModuleGraph entityPile
        
        flip mapM_ (zip [0..] $ Map.toList byModule) $
            \(index, (mod, entityID)) -> do
                case mod of
                    FrameworkModule _ _ -> return ()
                    LocalModule modName -> do
                        let entities = map (\eid -> (eid, lookupEntity "main" eid entityPile)) $
                                       Set.toList $ entityID
                        createParentDirectoriesIfNecessary (hsBootPathForModule modName)
                        writeFileIfChanged (hsBootPathForModule modName) $
                            show $ pprHsBoot entityPile modName entities
                        writeFileIfChanged (hsModulePathForModule modName) $
                            show $ pprHsModule entityPile modGraph modName entities
                        reportProgress progress nModules

readFileWithProgress progress fn
    = do
        bs <- BS.readFile fn
        let n = BS.length bs
        return $ monitorList progress n $ BS.unpack bs

#ifdef BINARY_INTERFACES
decodeFileWithProgress progress fn
    = do
        bs <- fmap LBS.toChunks $ LBS.readFile fn
        let n = length bs
        return $ decode $ LBS.fromChunks $ monitorList progress n $ bs
#endif

readInterfaceFileWithProgress progress fn
#ifdef BINARY_INTERFACES
    = decodeFileWithProgress progress fn
#else
    = fmap read $ readFileWithProgress progress fn
#endif

writeInterfaceFileWithProgress progress fn entities
#ifdef BINARY_INTERFACES
    = encodeFile fn $
        monitor progress $ localEntities $ entities
#else
    = writeFileIfChanged fn $ 
        show $ monitor progress $ localEntities $ entities
#endif

data HeaderDirectory
    = FrameworkHeaders String
    | Headers String
        
data Options = Options {
        oFrameworkName :: String,
        oRequiredFrameworks :: [String],
        oHeaderDirectories :: [HeaderDirectory],
        oUmbrella :: Bool,
        oBindingScript :: Maybe String,
        oAdditionalCode :: Maybe String,
        oShowZapped :: Bool,
        oDumpInitial :: Bool,
        oQuiet :: Bool
    }

processFramework options -- bs frameworkName requiredFrameworks
    = do
        bs <- maybe (return emptyBindingScript) readBindingScript $ 
                oBindingScript options
        let requiredFrameworks = oRequiredFrameworks options
            frameworkName = oFrameworkName options
        
        putStrLn $ "*** Processing Framework " ++ frameworkName ++ " ***"
        
        let mkProgress n | oQuiet options = return dummyProgressReporter
                         | otherwise      = newProgressReporter n
        
        importProgress    <- mapM mkProgress $
                             map ("Importing " ++) requiredFrameworks
        parseProgress     <- mkProgress "Parsing Objective-C header files"
        initialProgress   <- mkProgress "Building initial entities"
        resolveProgress   <- mkProgress "Resolving cross-references"
        typeProgress      <- mkProgress "Converting types"
        zapProgress       <- mkProgress "Zapping unconvertable entities"
        expandProgress    <- mkProgress "Filling in additional instance declarations"
        combineProgress   <- mkProgress "Combining duplicate entities"
        eliminateProgress <- mkProgress "Eliminating redundant instances"
        outputProgress    <- mkProgress "Writing binding modules"
        masterProgress    <- mkProgress $ "Writing " ++ frameworkName ++ ".hs"
        exportProgress    <- mkProgress $ "Writing " ++ frameworkName ++ ".pi"
        multiProgress <- if oQuiet options then return dummyMultiProgress else
                            openMultiProgress $ parseProgress : importProgress ++ 
                                           [initialProgress, resolveProgress,
                                            typeProgress, zapProgress,
                                            expandProgress, combineProgress,
                                            eliminateProgress, outputProgress,
                                            masterProgress, exportProgress]
       
        headers <- fmap concat $ flip mapM (oHeaderDirectories options) $
                        \hd -> case hd of
                            FrameworkHeaders framework
                                -> headersForFramework framework
                            Headers path
                                -> headersIn path (oFrameworkName options)

        loaded <- loadHeaders parseProgress headers
        
        importedEMaps <- mapM (\(fn, progress) ->
                                readInterfaceFileWithProgress progress
                                    ("HOC-" ++ fn ++ "/" ++ fn ++ ".pi")
                              )
                              (zip requiredFrameworks importProgress)
        
        let importedEntities
                = foldr (\(modName, eMap) pile
                        -> addImportedEntities modName 
                                (renameToFramework eMap modName) pile)
                        emptyEntityPile $
                        zip (map BS.pack requiredFrameworks) importedEMaps
        
        let initialEntities = monitor initialProgress $ makeEntities bs loaded importedEntities
        

        additionalEntities <-
            maybe (return initialEntities)
                  (\additionalCode ->
                        loadAdditionalCode additionalCode
                                           (map (\(_,_, modName) -> modName) headers)
                                           initialEntities)
                  (oAdditionalCode options)
        
        let resolvedEntities = monitor resolveProgress $ resolveReferences additionalEntities
            typedEntities = monitor typeProgress $ resolveTypes resolvedEntities
            (zappedEntities, zapMessages) = runMessages $ zapAndReportFailedTypes zapProgress typedEntities
            expandedEntities = monitor expandProgress $ expandProtocolRequirements zappedEntities
            combinedEntities = monitor combineProgress $ combineDulicateEntities expandedEntities
            finalEntities = renameClashingIdentifiers $ eliminateSubclassInstances eliminateProgress combinedEntities
            
        do
            let packageName = "HOC-" ++ frameworkName
            createDirectoryIfNecessary packageName
            
            writeFrameworkModules outputProgress finalEntities (packageName ++ "/")
            
            
            -- mapM_ print zapMessages
            writeFileIfChanged (packageName ++ "/" ++ frameworkName ++ ".hs") $ show $
                pprMasterModule (oUmbrella options)
                                (finalEntities)
                                (BS.pack frameworkName)
            writeFileIfChanged (packageName ++ "/" ++ packageName ++ ".cabal") $ show $
                pprCabalFile frameworkName requiredFrameworks finalEntities
            writeFileIfChanged (packageName ++ "/" ++ "Setup.hs") $
                "import Distribution.Simple\nmain = defaultMain\n"
            
            writeInterfaceFileWithProgress exportProgress
                                           (packageName ++ "/" ++ frameworkName ++ ".pi")
                                           finalEntities
          `finally` do
                
            closeMultiProgress multiProgress
            
            when (oDumpInitial options) $ do
                putStrLn "Initial entities:"
                mapM_ print $ Map.toList $ epEntities initialEntities

            when (oShowZapped options && not (null zapMessages)) $ do
                putStrLn "Zapped entities:"
                mapM_ print zapMessages
        
        putStrLn $ "done."

        
        
addRequiredFramework fw o
    = o { oRequiredFrameworks = fw : oRequiredFrameworks o }
addHeaderDirectory hd o
    = o { oHeaderDirectories = hd : oHeaderDirectories o }
 
optionDescs = [
        Option ['d'] ["depend"]
            (ReqArg addRequiredFramework
                    "framework") 
            "depend on framework",
        Option ['f'] ["framework"]
            (OptArg (\mbArg o -> addHeaderDirectory
                                    (FrameworkHeaders $
                                     fromMaybe (oFrameworkName o) mbArg)
                                    o
                                 )
                    "framework")
            "generate bindings for framework",
        Option ['I'] ["headers"]
            (ReqArg (\path o -> addHeaderDirectory
                                    (Headers path)
                                    o
                                 )
                    "path")
            "generate bindings for headers at path",
        Option ['u'] ["umbrella"]
            (NoArg (\o -> o { oUmbrella = True }))
            "reexport all imported frameworks",
        Option ['b'] ["binding-script"]
            (ReqArg (\bs o -> o { oBindingScript = Just bs })
                    "script")
            "use binding script",
        Option ['a'] ["additional-code"]
            (ReqArg (\ac o -> o { oAdditionalCode = Just ac })
                    "path")
            "additional code directory",
        Option ['z'] ["show-zapped"]
            (NoArg (\o -> o { oShowZapped = True }))
            "print messages about entities that couldn't be translated",
        Option [] ["dump-initial"]
            (NoArg (\o -> o { oDumpInitial = True }))
            "dump all entities after parsing",
        Option ['q'] ["quiet"]
            (NoArg (\o -> o { oQuiet = True }))
            "don't report progress"
    ]
main = do
    args <- getArgs
    case getOpt Permute optionDescs args of
        (optionsF, [frameworkName], []) ->
            let options0 = Options {
                        oFrameworkName = frameworkName,
                        oRequiredFrameworks = [],
                        oHeaderDirectories = [],
                        oUmbrella = False,
                        oBindingScript = Nothing,
                        oAdditionalCode = Nothing,
                        oShowZapped = False,
                        oDumpInitial = False,
                        oQuiet = False
                    }
                options = foldl (flip ($)) options0 optionsF
            in
                processFramework options
        (_, _, es) -> mapM_ putStrLn es >> putStrLn (usageInfo "hoc-ifgen [options] framework_name" optionDescs)
    
