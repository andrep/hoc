module Output(
        pprHsBoot,
        pprHsModule,
        pprMasterModule,
        pprCabalFile
    ) where

import Entities
import DependenceGraphs
import CTypeToHaskell
import HOC.NameCaseChange

import Control.Monad        ( guard )
import Data.Maybe           ( maybeToList )
import Text.PrettyPrint.HughesPJ

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

textBS = text . BS.unpack

idsForEntity e
    = case eInfo e of
        ClassEntity _ ->
            [eHaskellName e, '_' `BS.cons` eHaskellName e,
             eHaskellName e `BS.append` BS.pack "Class",
             eHaskellName e `BS.append` BS.pack "MetaClass",
             BS.pack "super_" `BS.append` eHaskellName e,
             eHaskellName e `BS.snoc` '_' ]
        EnumEntity complete values ->
            case eName e of
                CName _ | complete
                    -> (eHaskellName e `BS.append` BS.pack "(..)")
                        : lowercaseNames
                        | otherwise
                    -> eHaskellName e : lowercaseNames
                Anonymous -> lowercaseNames
            where
                lowercaseNames = map (BS.pack . nameToLowercase . BS.unpack . fst) values
                
        StructEntity _ _ -> [eHaskellName e `BS.append` BS.pack "(..)"]

        AdditionalCodeEntity _ exp _ _ -> exp
                
        _ -> case eName e of
                CName _ -> [eHaskellName e]
                ProtocolName _ -> [eHaskellName e]
                SelectorName _
                    -> [eHaskellName e, BS.pack "Has_" `BS.append` eHaskellName e,
                        BS.pack "info_" `BS.append` eHaskellName e,
                        BS.pack "ImpType_" `BS.append` eHaskellName e]
                _       -> []

pprSrcImportClass e
    = case eModule e of
        LocalModule m
            -> text "import {-# SOURCE #-}" <+> textBS m
                <+> parens (textBS (eHaskellName e) <> comma
                            <+> textBS (eHaskellName e) <> text "Class" <> comma
                            <+> textBS (eHaskellName e) <> text "MetaClass")
        FrameworkModule f m
            -> text "import" <+> textBS m
                <+> parens (textBS (eHaskellName e) <> comma
                            <+> textBS (eHaskellName e) <> text "Class" <> comma
                            <+> textBS (eHaskellName e) <> text "MetaClass")

pprHsBoot entityPile modName entities
    = text "module" <+> textBS modName <+> text "where" $+$
      text "import HOC" $+$
      vcat imports $+$
      vcat classes
    where
        classes0 = [ (name, fmap (flip (lookupEntity "pprHsBoot") entityPile) mbSuper)
                   | (_, Entity { eHaskellName = name,
                                  eInfo = ClassEntity mbSuper }) <- entities ]

        classes = [ text "data" <+> textBS name <> char '_' <+> char 'a' $+$
                    text "type" <+> textBS name <+> char 'a' <+> equals
                        <+> text (maybe "ID" (BS.unpack . eHaskellName) mbSuper)
                        <+> parens (textBS name <> char '_' <+> char 'a') $+$
                    text "type" <+> textBS name <> text "Class" <+> char 'a' <+> equals
                        <+> text (maybe "Class" ( (++ "Class") . BS.unpack . eHaskellName ) mbSuper)
                        <+> parens (textBS name <> char '_' <+> char 'a') $+$
                    text "type" <+> textBS name <> text "MetaClass" <+> char 'a' <+> equals
                        <+> text (maybe "MetaClass" ( (++ "MetaClass") . BS.unpack . eHaskellName ) mbSuper)
                        <+> parens (textBS name <> char '_' <+> char 'a')
                    
                  | (name, mbSuper) <- classes0 ]
            
        imports = do
            (_, mbSuper) <- classes0
            super <- maybeToList mbSuper
            guard (eModule super /= LocalModule modName)
            return $ pprSrcImportClass super
        
-- *****************************************************************************

pprHsModule entityPile modGraph modName idsAndEntities
    = text "{-# OPTIONS -fth -fglasgow-exts #-}" $+$
      text "module" <+> textBS modName
        <> parens (exportList) <+> text "where" $+$
      text "import HOC" $+$
      text "import qualified Prelude" $+$
      text "import Prelude (Bool, IO, Float, Double)" $+$
      text "import Foreign.C.Types" $+$     -- ###
      text "import Foreign (Ptr)" $+$     -- ###
      importList $+$
      additionalImports $+$
      vcat (map pprEntity $ map snd $ topsortEntities entityPile $ idsAndEntities)
    where
        lookupE = flip (lookupEntity "pprHsModule") entityPile
        
        --compareInfos a b = compare (eInfo a) (eInfo b)
        -- sortBy compareInfos
        
        entities = map snd idsAndEntities
        
        exportList = vcat $ punctuate comma $ 
                     (text "module HOC" :) $ map textBS $
                     Set.toList $ Set.fromList $ concatMap idsForEntity entities

        importList = vcat $ map pprEntityImport importedEntities

        importedEntities = 
            filter ( (/= LocalModule modName) . eModule ) $
            map lookupE $
            Set.toList $ Set.unions $ map Set.fromList $
            map entitiesRequiredByEntity entities
        
        additionalImports = vcat [ textBS imp
                                 | Entity { eInfo = AdditionalCodeEntity _ _ imp _ }
                                   <- entities ]
        
        pprEntityImport e
            = case eInfo e of 
                ClassEntity _
                    | srcImport ->
                    pprSrcImportClass e
                _ ->
                    text "import" <+> textBS srcMod
                        <+> parens (hsep $ punctuate comma $ map textBS $ idsForEntity e)
            where
                (srcMod, srcImport)
                    = case eModule e of
                        LocalModule mn
                            -> (mn, isSourceImport (LocalModule modName)
                                                   (eModule e)
                                                   modGraph)
                        FrameworkModule fw mn -> (mn, False)
        
        pprEntity e@(Entity { eInfo = ClassEntity mbSuper })
            = text "$(declareClass" <+> doubleQuotes (textBS $ eHaskellName e)
                                    <+> doubleQuotes (textBS $ superClassName)
                                    <> text ")"
            where superClassName = maybe (BS.pack "ID")
                                         (eHaskellName . lookupE)
                                         mbSuper
        
        pprEntity e@(Entity { eInfo = SelectorEntity t })
            = text "$(declareRenamedSelector" <+>
                doubleQuotes (textBS name) <+>
                doubleQuotes (textBS $ eHaskellName e) <+>
                text "[t|" <+> pprSelectorType ht <+> text "|])"
            where SelectorName name = eName e
                  ConvertedType ht _ = t

        pprEntity e@(Entity { eInfo = ProtocolEntity supers selectors })
            = text "class" <+> context <+> textBS (eHaskellName e) <+> text "a"
            where
                context | not (null classes) = (parens $ cat $ punctuate comma $
                                                map (\cls -> textBS cls <+> text "a") $
                                                classes) <+> text "=>"
                                               
                        | otherwise = empty
                classes = map (eHaskellName . lookupE) supers
                       ++ map ((BS.pack "Has_" `BS.append`) . eHaskellName . lookupE) selectors

        pprEntity e@(Entity { eInfo = MethodEntity })
            = text "instance" <+> text "Has_" <> textBS (eHaskellName (lookupE sel))
                <+> parens (textBS (eHaskellName (lookupE cls))  <> classSuffix <+> text "a")
            where
                SelectorInstanceName cls sel isFactory = eName e
                classSuffix | isFactory = text "Class"
                            | otherwise = empty

        pprEntity e@(Entity { eInfo = ProtocolAdoptionEntity })
            = text "instance" <+> textBS (eHaskellName (lookupE proto))
                <+> parens (textBS (eHaskellName (lookupE cls)) <+> text "a")
            where
                ProtocolAdoptionName cls proto = eName e

        -- pprEntity e@(Entity { eInfo = EnumEntity constants })
        --    = empty
        pprEntity e@(Entity { eInfo = TypeSynonymEntity t })
            = text "type" <+> textBS (eHaskellName e) <+> equals <+> pprVariableType ht
            where
                ConvertedType ht _ = t

        pprEntity e@(Entity { eName = CName cn, eInfo = ExternVarEntity t })
            = text "$(declareExternConst" <+> doubleQuotes (textBS cn)
                                          <+> text "[t|" <+> pprVariableType ht <+> text "|])"
            where
                ConvertedType ht _ = t

        pprEntity e@(Entity { eName = CName cn, eInfo = ExternFunEntity t })
            = text "$(declareExternFun" <+> doubleQuotes (textBS cn)
                                        <+> text "[t|" <+> pprSelectorType ht <+> text "|])"
            where
                ConvertedType ht _ = t

        pprEntity e@(Entity { eInfo = EnumEntity complete constants })
            = char '$' <> parens (
                    declare <+> brackets (
                        hcat $ punctuate comma $ map pprAssoc constants
                    )
                )
            where
                declare = case eName e of
                    CName cname -> text "declareCEnum" <+> doubleQuotes (textBS cname)
                    Anonymous   -> text "declareAnonymousCEnum"
                pprAssoc (n, v)
                    = parens (doubleQuotes (textBS n) <> comma <+> integer v)

        pprEntity e@(Entity { eInfo = StructEntity mbTag fields })
            = char '$' <> parens (
                    declare <+> brackets (
                        hcat $ punctuate comma $ map pprType fields
                    )
                )
            where
                declare = case eName e of
                    CName cname -> text "declareCStructWithTag"
                                    <+> doubleQuotes (textBS cname)
                                    <+> tag
                tag = case mbTag of Nothing -> text "Prelude.Nothing"
                                    Just t -> parens (text "Prelude.Just" <+> doubleQuotes (text t))
                
                pprType t = text "[t|" <+> pprVariableType ht <+> text "|]"
                    where ConvertedType ht _ = t

        pprEntity e@(Entity { eInfo = AdditionalCodeEntity _ _ _ txt })
            = textBS txt

        pprEntity e = empty
        

pprMasterModule umbrella entityPile frameworkName
    = text "module" <+> textBS frameworkName
        <> parens (exportList) <+> text "where" $+$
      text "import Prelude ()" $+$
      text "import HOC" $+$
      importList
    where
        allEntities = filter (not . isReexported . snd) $
                      filter (isLocal . fst) $
                      entityPileToList entityPile
        
        entities
            = Map.elems $ Map.fromList $
                [ (eid, e)
                | (_, [(eid, e)])
                  <- Map.toList $ Map.fromListWith (++) $
                     [ (n, [(eid, e)])
                     | (eid, e) <- allEntities,
                       n <- idsForEntity e ] ]
        
        isLocal (LocalEntity _) = True
        isLocal _ = umbrella
        
        isReexported (Entity { eInfo = ReexportEntity eid }) 
            = isLocal eid
        isReexported _ = False
        
        exportList = vcat $ punctuate comma $ (text "module HOC" :) $ map textBS $
                     Set.toList $ Set.fromList $ concatMap idsForEntity entities

        importList = vcat $ map pprEntityImport entities

        pprEntityImport e
            = text "import" <+> textBS srcMod
                        <+> parens (hsep $ punctuate comma $ map textBS $ idsForEntity e)
            where
                srcMod = case eModule e of
                    LocalModule mn -> mn
                    FrameworkModule fw mn -> mn
   
   
pprCabalFile frameworkName dependencies entities
    = text "name:" <+> text "HOC-" <> text frameworkName $+$
      text "version: 1.0" $+$
      text "build-type: Simple" $+$
      text "build-depends:" <+>
        hsep (punctuate comma $ map text $
                ["base", "HOC"] ++ map ("HOC-" ++) dependencies) $+$
--      text "" $+$
      text "exposed-modules:" <+> sep (punctuate comma $
                                        map textBS $ BS.pack frameworkName : modules) $+$
      text "frameworks:" <+> text frameworkName
    where
        modules = [ m | LocalModule m
                   <- Set.toList $ Set.fromList $
                      map eModule $ Map.elems $ localEntities entities ]

