module ExportModule(
        exportModule,
        idsForSel
    ) where

import Files
import CTypeToHaskell
import PrepareDeclarations
import BindingScript
import Utils(groupByFirst)
import Headers(ModuleName)

import Data.Set
import qualified Data.HashTable as HashTable
import Data.List(nub, partition)
import Data.Maybe(catMaybes, maybeToList, isNothing)
import Text.PrettyPrint.HughesPJ

orderClassInfos [] = []
orderClassInfos cis = ok ++ orderClassInfos notOK
    where
        (notOK, ok) = partition (\ci -> ciSuper ci `elem` justNames) cis
        justNames = map (Just . ciName) cis

exportModule bindingScript 
             (PreparedDeclarations {
                 pdCleanClassInfos = cleanClassInfos,
                 pdCleanClassInfoHash = cleanClassInfoHash,
                 pdAllInstanceSels = allInstanceSels,
                 pdAllClassSels = allClassSels
             })
             selsDefinedWhere
             noteSelDefinition
             moduleName = do
    
    let outputFileName  = outputPath $ dotToSlash moduleName ++ ".hs"
        forwardDirName  = outputPath $ dotToSlash moduleName ++ "/"
        forwardFileName = outputPath $ dotToSlash moduleName ++ "/Forward.hs"
        
        dotToSlash = map f
            where
                f '.' = '/'
                f c = c
                
    -- putStrLn $ "Exporting " ++ outputFileName

    let definedClassInfos = [ ci | (_,ci) <- cleanClassInfos, ciDefinedIn ci == moduleName ]
        definedClassNames = map ciName definedClassInfos
    
    let makeClassImports classes = do
            infos <- mapM (HashTable.lookup cleanClassInfoHash) classes
            return $ map (\(mod, ids) -> (mod, concatMap idsForClass ids)) $
                     groupImports moduleName $
                     map (\ci -> (ciDefinedIn ci,ciName ci)) $
                     catMaybes $
                     infos
        importsToForward = map (\(mod, ids) -> (mod ++ ".Forward", ids))
        makeForwardClassImports = fmap (importsToForward) .
                                  makeClassImports

    let superClasses = nub $ catMaybes $ map ciSuper definedClassInfos

    superClassImports <- makeClassImports superClasses
    
    let superClassForwardImports = importsToForward superClassImports
        superClassModules = map fst superClassImports
                     
    let protocolsToDeclare =
            filter (\(ci, sels) -> ciProtocol ci
                                && ciDefinedIn ci == moduleName) $ 
            allInstanceSels

    let makeProtocolImports protos = do
            infos <- mapM (HashTable.lookup cleanClassInfoHash) protos
            return $ map (\(mod, ids) -> (mod, concatMap idsForProto ids)) $
                     groupImports moduleName $
                     map (\ci -> (ciDefinedIn ci,ciName ci)) $
                     catMaybes $
                     infos

    adoptedProtoImports <- makeProtocolImports $
                           map (++ "Protocol") $
                           setToList $
                           unionManySets $
                           map ciNewProtocols $
                           definedClassInfos

    let selsToDefineOrImport = nub (concatMap snd instanceSelsToDefine
                                    ++ concatMap snd classSelsToDefine)
                        
        
        instanceSelsToDefine = relevantSels slDefinitionLocation allInstanceSels
        classSelsToDefine = relevantSels slDefinitionLocation allClassSels

        instanceSels = relevantSels slHasInstanceLocation 
                [ (ciName ci, sels) | (ci,sels) <- allInstanceSels, not (ciProtocol ci) ]
        classSels = relevantSels slHasInstanceLocation
                [ (ciName ci, sels) | (ci,sels) <- allClassSels, not (ciProtocol ci) ]

        importedProtocolSels = concat [ [ (def, idsForSel (msMangled mangledSel))
                                        | (mangledSel, SelectorLocation def inst) <- sels,
                                          inst == moduleName, def /= moduleName ] 
                                      | (ci,sels) <- allInstanceSels ++ allClassSels,
                                                     not (ciProtocol ci) ]


        relevantSels :: (SelectorLocation -> ModuleName)
                     -> [ (a, [(MangledSelector, SelectorLocation)]) ]
                     -> [ (a, [MangledSelector]) ]
        relevantSels f = pruneEmptySnd . map g
            where g (cls, sels) = (cls,
                                   [ sel | (sel, loc) <- sels,
                                           f loc == moduleName ])
                  
        
        pruneEmptySnd xs = [ (a,b) | (a, b@(_:_)) <- xs ]
        
         
                            
    selsToDefine <- mapM (\mangledSel -> do
            maybeMod <- HashTable.lookup selsDefinedWhere (msSel mangledSel)
            case maybeMod of
                Just otherMod -> do
                    noteSelDefinition False mangledSel moduleName
                    return $ Left (otherMod, mangledSel)
                Nothing -> do
                    HashTable.insert selsDefinedWhere (msSel mangledSel) moduleName
                    noteSelDefinition True mangledSel moduleName
                    return $ Right mangledSel
        ) selsToDefineOrImport
        
    let selImports = [ (otherMod, idsForSel (msMangled mangledSel))
                     | Left (otherMod, mangledSel) <- selsToDefine ]
        selDefinitions = [ mangledSel
                         | Right mangledSel <- selsToDefine ]
        
        methodInstances :: [ (String, String) ]
        methodInstances = mkDecls id instanceSels
                       ++ mkDecls (++ "Class") classSels
            where
                mkDecls f classesAndSels = concat [ [(msMangled sel, f cls) | sel <- sels ]
                                                  | (cls, sels) <- classesAndSels ]
                        
        exportedSelNames = map msMangled selsToDefineOrImport
        exportedSels = concatMap idsForSel exportedSelNames
        exportedSelSet = mkSet exportedSelNames

        exportedClasses = concat [ idsForClass $ ciName ci
                                 | ci <- definedClassInfos,
                                   not (ciProtocol ci) ]
        exportedProtos = concat [ idsForProto $ ciName ci
                                | ci <- definedClassInfos,
                                  ciProtocol ci ]
        exportedIds = exportedClasses ++ exportedSels
            

        protoAdoptions = concat [ [ (proto ++ "Protocol", ciName ci)
                                  | proto <- setToList $ ciNewProtocols ci]
                                | ci <- definedClassInfos, not (ciProtocol ci) ]
            
    let mentionedClassNames = nub $ concatMap (mentionedClasses . msType) selDefinitions                
    mentionedClassImports <- makeForwardClassImports mentionedClassNames

    categoryImports <- makeForwardClassImports $ setToList $ mkSet $ map fst (instanceSels ++ classSels)

    additionalCode <- readFileOrEmpty (additionalCodePath (dotToSlash moduleName ++ ".hs"))

    let anythingGoingOn = not $ and [null methodInstances,
                                     null exportedClasses,
                                     null exportedSels,
                                     null exportedProtos,
                                     isNothing additionalCode]
        
        additionalCodeLines = lines $ concat $ maybeToList additionalCode
        
        forwardModule = render $ vcat $ [
                text "module " <+> text (moduleName ++ ".Forward")
                    <+> parens (sep $ punctuate comma $ map text exportedClasses)
                    <+> text "where",
                text "import HOC"
            ] 
            ++ map pprImport superClassForwardImports
            ++ map pprClassDecl (orderClassInfos $ filter (not . ciProtocol) $ definedClassInfos)
            
        declarationModule = render $ vcat $ [
                text "module" <+> text moduleName
                    <+> parens (sep $ punctuate comma $
                        map text (("module " ++ moduleName ++ ".Forward")
                                 : "module HOC"
                                 : exportedSels ++ exportedProtos
                                 ++ map ("module "++) superClassModules
                                 ++ [additionalExport | '-':'-':'X':additionalExport <- additionalCodeLines ]))
                    <+> text "where",
                text "import Prelude hiding" <+>
                    parens (sep $ punctuate comma $ map text $ setToList $
                            bsHiddenFromPrelude bindingScript `intersect` exportedSelSet),
                text "import Foreign.C.Types",
                text "import Foreign.Ptr",
                text "import HOC",
                text "import" <+> text (moduleName ++ ".Forward")
            ]
            ++ (map text $ takeWhile (/= "-- CUT HERE") additionalCodeLines)
            ++ [text "-- superclasses"]
            ++ map pprImport superClassImports
            -- ++ map pprImportAll superClassModules
            ++ [text "-- adopted protocols"]
            ++ map pprImport adoptedProtoImports
            ++ [text "-- classes mentioned in type signatures"]                    
            ++ map pprImport mentionedClassImports
            ++ [text "-- selectors that are reexported"]
            ++ map pprImport selImports
            ++ [text "-- selectors from adopted protocols"]
            ++ map pprImport importedProtocolSels
            ++ [text "-- classes for which we have categories here"]
            ++ map pprImport categoryImports
            ++ [text "-- selectors"]
            ++ map pprSelDecl selDefinitions
            ++ [text "-- methods"]
            ++ map pprMethodDecl methodInstances
            ++ [text "-- protocols"]
            ++ map pprProtocolDecl protocolsToDeclare
            ++ [text "-- protocol adoptions"]
            ++ map pprProtoAdoption protoAdoptions
            ++ (map text $ dropWhile (/= "-- CUT HERE") additionalCodeLines)
    

        
    if anythingGoingOn
        then do
            createDirectoryIfNecessary forwardDirName
            writeFileIfChanged outputFileName declarationModule
            writeFileIfChanged forwardFileName forwardModule
            putStrLn $ "Wrote " ++ outputFileName
            return $ Just moduleName
        else do
            putStrLn $ "Nothing to export to " ++ outputFileName
            return $ Nothing



groupImports :: ModuleName -> [(ModuleName, String)] -> [(ModuleName, [String])]
groupImports thisModule = filter (\(mod, _) -> mod /= thisModule) . groupByFirst
                          
idsForClass :: String -> [String]
idsForClass name = [name, "_" ++ name, name ++ "Class", "super_" ++ name]

idsForSel :: String -> [String]
idsForSel name = [name, "Has_" ++ name, "info_" ++ name, "ImpType_" ++ name]

idsForProto :: String -> [String]
idsForProto name = [name]

pprImportAll :: ModuleName -> Doc
pprImportAll mod = text "import" <+> text mod

pprImport :: (ModuleName, [String]) -> Doc
pprImport (mod, ids) = text "import" <+> text mod <> parens (cat $ punctuate comma $ map text ids)

pprClassDecl :: ClassInfo -> Doc
pprClassDecl ci = text "$" <> parens (text "declareClass"
        <+> doubleQuotes (text $ ciName ci)
        <+> doubleQuotes (text $ case ciSuper ci of Nothing -> "ID" ; Just x -> x)
    )

pprProtocolDecl :: (ClassInfo, [(MangledSelector, SelectorLocation)]) -> Doc
pprProtocolDecl (ci,selsAndLocs) =
        text "class" <+> context <+>
        text (ciName ci) <+> text "a"
    where
        context | not (null classes) = (parens $ cat $ punctuate comma $
                                        map (\cls -> text cls <+> text "a") $
                                        classes) <+> text "=>"
                                       
                | otherwise = empty
        classes =    map (++ "Protocol") (setToList $ ciNewProtocols ci)
                  ++ map (("Has_" ++) . msMangled . fst) selsAndLocs

pprSelDecl :: MangledSelector -> Doc
pprSelDecl mangledSel = text "$" <> parens (text "declareRenamedSelector"
        <+> doubleQuotes (text $ msName mangledSel)
        <+> doubleQuotes (text $ msMangled mangledSel)
        <+> text "[t|" <+> pprSelectorType (msType mangledSel) <+> text "|]"
    )

pprMethodDecl :: (String, String) -> Doc
pprMethodDecl (selName, className) =
    text "instance" <+> text ("Has_" ++ selName) <+> parens (text className <+> text "a")

pprProtoAdoption :: (String, String) -> Doc
pprProtoAdoption (protoName, className) =
    text "instance" <+> text protoName <+> parens (text className <+> text "a")
