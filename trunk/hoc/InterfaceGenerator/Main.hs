module Main where

import Control.Exception(evaluate)

import Data.FiniteMap
import qualified Data.HashTable as HashTable
import Data.List(isPrefixOf,isSuffixOf,partition)
import Data.Maybe(fromMaybe,mapMaybe,isJust,isNothing,catMaybes,maybeToList)
import Data.Set
import Control.Monad(unless)

import System.Info(os)

import Text.PrettyPrint.HughesPJ

import SyntaxTree
import CTypeToHaskell
import BindingScript
import PrepareDeclarations
import Files
import Utils
import ExportModule
import Headers
import Enums(extractEnums)


writeMasterModule masterModuleName realModuleNames selNamesList = do
    let conflictingDecls = listToFM $
                           map (\(mod,sels) -> (mod, concatMap idsForSel sels)) $
                           groupByFirst $
                           concatMap (\(selName,(cnt,exporters)) ->
                                           if cnt > 1
                                               then [(mod,msMangled ms) | (ms,mod) <- exporters]
                                               else []
                                     ) $
                           selNamesList
        hidingClause mod = case lookupFM conflictingDecls mod of
            Just decls -> text "hiding" <+> parens (sep $ punctuate comma $ map text $ decls)
            Nothing -> empty
        
        moduleNames = "HOC" : realModuleNames
        
        masterModule = render $ vcat $ [
                text "{-# OPTIONS -fno-warn-duplicate-exports #-}",
                text "module" <+> text masterModuleName
                    <+> parens (sep $ punctuate comma $
                                map ( (text "module" <+>) . text ) moduleNames)
                    <+> text "where"
            ]
            ++ map (\mod -> text "import" <+> text mod <+> hidingClause mod) moduleNames
    writeFileIfChanged (outputPath $ masterModuleName ++ ".hs") masterModule
    

main = do
    bindingScript <- readBindingScript "binding-script.txt"
    
    if System.Info.os == "darwin"
        then createOutputDirectories ["Foundation", "AppKit"]
        else createOutputDirectories ["Foundation", "AppKit", "GNUstepBase", "GNUstepGUI"]
    
    foundationHeaders <-
        if System.Info.os == "darwin"
            then headersForFramework "Foundation"
            else fmap concat $ mapM headersForFramework ["Foundation", "GNUstepBase"]
            
    appKitHeaders <-
        if System.Info.os == "darwin"
            then headersForFramework "AppKit"
            else fmap concat $ mapM headersForFramework ["AppKit", "GNUstepGUI" ]
    foundationModules <- loadHeaders foundationHeaders
    appKitModules <- loadHeaders appKitHeaders
    
    preparedDeclarations <- prepareDeclarations bindingScript (foundationModules ++ appKitModules)
 
    let headerNames = map (\(HeaderInfo name _ _) -> name)
    
    let orderModules2 mods = do
            deps <- mapM (getModuleDependencies preparedDeclarations) mods
            let order [] = []
                order xs | null ok = error $ "nothing OK:" ++ show xs
                         | otherwise = ok ++ order notOK
                    where (notOK, ok) = partition (\(name,imports) -> any (`elem` names) imports) xs
                          names = map fst xs
            return $ map fst $ order $ zip mods deps
            
    modules <- fmap concat $ mapM (orderModules2 . headerNames) [foundationModules, appKitModules]
            
    selsDefinedWhere <- HashTable.new (==) (\sel -> HashTable.hashString (selName sel))
    
    allSelNames <- HashTable.new (==) HashTable.hashString
    
    let noteSelDefinition new sel mod = do
            (nDefs, exporters) <- fmap (fromMaybe (0,[])) $ HashTable.lookup allSelNames (msMangled sel)
            HashTable.delete allSelNames (msMangled sel)
            HashTable.insert allSelNames (msMangled sel) (if new then nDefs + 1 else nDefs, (sel,mod):exporters)            
   
    realModuleNames <- fmap catMaybes $ 
                       mapM ( exportModule bindingScript
                                           preparedDeclarations
                                           selsDefinedWhere
                                           noteSelDefinition ) $
                       modules
    
    selNamesList <- HashTable.toList allSelNames
    
        -- this is cheap: it would be nicer to generate the bindings for
        -- AppKit & Foundation separately, and without hard-coded names.
    let writeMasterModule' name = writeMasterModule name
                                                    (filter ((name++".") `isPrefixOf`) realModuleNames)
                                                    selNamesList
    writeMasterModule' "Foundation"
    unless (System.Info.os == "darwin") $ writeMasterModule' "GNUstepBase"
    writeMasterModule' "AppKit"
    unless (System.Info.os == "darwin") $ writeMasterModule' "GNUstepGUI"
    writeMasterModule "Cocoa" realModuleNames selNamesList
    
    let manglingConflicts :: [(String, [(String, [ModuleName])])]
        manglingConflicts = filter ((>1) . length . snd) $
                            map (\(selName, mangledSelsAndExporters) ->
                                  (selName, groupByFirst $ mapFst msName $
                                            mangledSelsAndExporters)) $
                            mapSnd snd $
                            filter ((>1) . fst . snd) $
                            selNamesList
                            
        typeConflicts :: [(String, [(HSelectorType, [ModuleName])])]
        
        typeConflicts = filter ((>1) . length . snd) $
                        mapSnd (groupByFirst . mapFst msType) $
                        map (\(_, selsAndExps@((sel,exp):_)) -> (msName sel, selsAndExps)) $
                        groupByFirst $
                        map (\(sel,exp) -> (msName sel, (sel, exp))) $
                        concatMap (snd . snd) $
                        filter ((>1) . fst . snd) $
                        selNamesList

    putStrLn $ render (text "Conflicts caused by types:" $$ (nest 4 $
            vcat $ map (\(name, types) ->
                            text name <+> (vcat $
                                map (\(typ, mods) ->
                                        parens (pprSelectorType typ)
                                        <+> hsep (map text mods)
                                    ) types)
                    ) typeConflicts
        ))
                            
    putStrLn $ render (text "Conflicts caused by name mangling:" $$ (nest 4 $
            vcat $ map (\(mangled, originals) ->
                            text mangled <+> (vcat $
                                map (\(original, mods) ->
                                        text original <+> hsep (map text mods)
                                    ) originals)
                    ) manglingConflicts
        ))

    writeFile "all-selectors.txt" $
        unlines $
        map show $
        map (\(sel,mod) -> (msMangled sel,
                            msName sel,
                            render $ pprSelectorType $ msType sel,
                            mod)) $
        concatMap (snd . snd) $
        selNamesList
