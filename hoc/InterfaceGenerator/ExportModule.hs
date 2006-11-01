module ExportModule(
        exportModule,
        getModuleDependencies,
        idsForSel
    ) where

import Files
import CTypeToHaskell
import PrepareDeclarations
import BindingScript
import Utils(groupByFirst)
import Headers(ModuleName)
import Enums(enumName, pprEnumType)

import HOC.NameCaseChange

import Data.Set(Set)
import qualified Data.Set as Set hiding (Set)
import qualified Data.HashTable as HashTable
import Data.List(nub, partition, isPrefixOf, group, sort)
import Data.Maybe(fromMaybe, catMaybes, mapMaybe, maybeToList, isNothing)
import qualified Data.Map as Map (lookup, findWithDefault) 
import Text.PrettyPrint.HughesPJ

getModuleDependencies :: PreparedDeclarations -> ModuleName -> IO [ModuleName]
getModuleDependencies (PreparedDeclarations { 
                        pdCleanClassInfos = cleanClassInfos,
                        pdCleanClassInfoHash = cleanClassInfoHash
                      })
                      moduleName = do
    let definedClassInfos = [ ci | (_,ci) <- cleanClassInfos, ciDefinedIn ci == moduleName ]
        superClasses = nub $ catMaybes $ map ciSuper definedClassInfos
        adoptedProtocols = map (++ "Protocol") $
                           Set.toList $
                           Set.unions $
                           map ciNewProtocols $
                           definedClassInfos
    
    infos <- mapM (HashTable.lookup cleanClassInfoHash) (superClasses ++ adoptedProtocols)
    let superModules = map ciDefinedIn $ catMaybes $ infos
    
    
    return $ nub $ filter (/= moduleName) $ superModules

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
                 pdAllClassSels = allClassSels,
                 pdEnumTypeDefinitions = allEnumDefinitions,
                 pdExternVarDeclarations = allVarDeclarations,
                 pdExternFunDeclarations = allFunDeclarations,
                 pdTypeEnvironment = typeEnv
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
                           Set.toList $
                           Set.unions $
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
        methodInstances = map head . group . sort $
                          instanceDecls
                       ++ classDecls
                       ++ nsObjectHackDecls
            where
                mkDecls f classesAndSels = concat [ [(msMangled sel, f cls) | sel <- sels ]
                                                  | (cls, sels) <- classesAndSels ]

                instanceDecls = mkDecls id instanceSels
                classDecls = mkDecls (++ "Class") classSels

                    -- HACK for NSObject.
                    -- NSObject is it's own meta-class; that's hard to model
                    -- in our type system, so we just automatically make every
                    -- instance method of NSObject a class method, too.
                nsObjectHackDecls = [ (sel, "NSObjectClass")
                                    | (sel, "NSObject") <- instanceDecls ]

        exportedSelNames = map msMangled selsToDefineOrImport
        exportedSels = concatMap idsForSel exportedSelNames
        exportedSelSet = Set.fromList exportedSelNames

        exportedClasses = concat [ idsForClass $ ciName ci
                                 | ci <- definedClassInfos,
                                   not (ciProtocol ci) ]
        exportedProtos = concat [ idsForProto $ ciName ci
                                | ci <- definedClassInfos,
                                  ciProtocol ci ]
        exportedIds = exportedClasses ++ exportedSels
            

        protoAdoptions = concat [ [ (proto ++ "Protocol", ciName ci)
                                  | proto <- Set.toList $ ciNewProtocols ci]
                                | ci <- definedClassInfos, not (ciProtocol ci) ]
                                
        varDeclarations = Map.findWithDefault [] moduleName allVarDeclarations
        funDeclarations = Map.findWithDefault [] moduleName allFunDeclarations
            
    let mentionedTypeNames = nub $
            concatMap (mentionedTypes.msType) (selDefinitions ++ funDeclarations)
            ++ concatMap (\(t,_,_) -> varMentionedTypes t) varDeclarations
                                
            
            -- ### we discard the information about where to import it from
            --     and then recover it later - not nice
        mentionedClassNames = filter (isClassType typeEnv) mentionedTypeNames
        
        
        mentionedTypeImports = importsToForward $
                               groupImports moduleName $
                               [ (loc, name)
                               | (name, Just (PlainTypeName, loc))
                                 <- map (\name -> (name, lookupTypeEnv typeEnv name))
                                        mentionedTypeNames ]
         
    mentionedClassImports <- makeForwardClassImports mentionedClassNames

    categoryImports <- makeForwardClassImports $ Set.toList $ Set.fromList $ map fst (instanceSels ++ classSels)

    additionalCode <- readFileOrEmpty (additionalCodePath (dotToSlash moduleName ++ ".hs"))

    let additionalCodeLines = lines $ concat $ maybeToList additionalCode
        [additionalCodeAbove, additionalCodeBelow,
         additionalCodeAboveForward, additionalCodeBelowForward]
            = take 4 $ (splitAt "-- CUT HERE" additionalCodeLines) ++ repeat []

        splitAt s xs 
                | null xs'' = [xs']
                | otherwise = xs' : splitAt s (tail xs'')
            where (xs', xs'') = span (not . (s `isPrefixOf`)) xs
            
        additionalExports = [ additionalExport
                            | '-':'-':'X':additionalExport
                              <- additionalCodeAbove ++ additionalCodeBelow ]
        additionalForwardExports = [ additionalExport
                                   | '-':'-':'X':additionalExport
                                     <- additionalCodeAboveForward
                                        ++ additionalCodeBelowForward ]

    let enumDefinitions = fromMaybe [] $ Map.lookup moduleName allEnumDefinitions

    let anythingGoingOn = not $ and [null methodInstances,
                                     null exportedClasses,
                                     null exportedSels,
                                     null exportedProtos,
                                     isNothing additionalCode,
                                     null enumDefinitions]
        
        forwardModule = render $ vcat $ [
                text "module " <+> text (moduleName ++ ".Forward")
                    <+> parens (sep $ punctuate comma $
                        map text (exportedClasses
                                 ++ [ nameToUppercase enum ++ "(..)"
                                    | enum <- mapMaybe enumName enumDefinitions ]
                                 ++ additionalForwardExports
                                 ))
                    <+> text "where",
                if null enumDefinitions then empty
                                        else text "import Foreign.C.Types(CInt)",
                text "import HOC"
            ] 
            ++ (map text $ additionalCodeAboveForward)
            ++ map pprImport superClassForwardImports
            ++ map pprClassDecl (orderClassInfos $ filter (not . ciProtocol) $ definedClassInfos)
            ++ [text "-- enum definitions"]
            ++ map pprEnumType enumDefinitions
            ++ (map text $ additionalCodeBelowForward)
            
        declarationModule = render $ vcat $ [
                text "module" <+> text moduleName
                    <+> parens (sep $ punctuate comma $
                        map text (("module " ++ moduleName ++ ".Forward")
                                 : "module HOC"
                                 : exportedSels ++ exportedProtos
                                 ++ map ("module "++) superClassModules
                                 ++ map (\(_,_,hn) -> hn) varDeclarations
                                 ++ map msMangled funDeclarations
                                 ++ additionalExports
                                 ))
                    <+> text "where",
                text "import Prelude hiding" <+>
                    parens (sep $ punctuate comma $ map text $ Set.toList $
                            bsHiddenFromPrelude bindingScript `Set.intersection` exportedSelSet),
                text "import Foreign.C.Types",
                text "import Foreign.Ptr",
                text "import HOC",
                text "import" <+> text (moduleName ++ ".Forward")
            ]
            ++ (map text $ additionalCodeAbove)
            ++ [text "-- superclasses"]
            ++ map pprImport superClassImports
            -- ++ map pprImportAll superClassModules
            ++ [text "-- adopted protocols"]
            ++ map pprImport adoptedProtoImports
            ++ [text "-- classes mentioned in type signatures"]                    
            ++ map pprImport mentionedClassImports
            ++ [text "-- plain types mentioned in type signatures"]
            ++ map pprImport mentionedTypeImports
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
            ++ [text "-- extern constants"]
            ++ map pprVarDecl varDeclarations
            ++ [text "-- extern functions"]
            ++ map pprFunDecl funDeclarations
            ++ (map text $ additionalCodeBelow)
    

         
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
idsForClass name = [name, "_" ++ name, name ++ "Class", "super_" ++ name
                        -- we also need to export the phantom type 
                        -- and a data constructor(!) for it, in order to
                        -- work around GHC bug #1244882.
                        , name ++ "_(..)"
                    ]

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
        classes =    map (++ "Protocol") (Set.toList $ ciNewProtocols ci)
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

pprVarDecl :: (HType, String, String) -> Doc
pprVarDecl (t, name, _) = text "$" <> parens (text "declareExternConst"
        <+> doubleQuotes (text name)
        <+> text "[t|" <+> pprVariableType t <+> text "|]"
    )

pprFunDecl :: MangledSelector -> Doc
pprFunDecl ms = text "$" <> parens (text "declareExternFun"
        <+> doubleQuotes (text $ msName ms)
        <+> text "[t|" <+> pprSelectorType (msType ms) <+> text "|]"
    )
