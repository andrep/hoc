{-# LANGUAGE TemplateHaskell #-}
module HOC.DeclareClass(declareClass) where

import HOC.Class
import HOC.Super

import HOC.TH

import Debug.Trace

import HOC.Arguments
import Foreign.Ptr
import System.IO.Unsafe

declareClass :: String -> String -> Q [Dec]

declareClass name super = sequence $ [
        -- data $(phantomName) a
        dataD (cxt []) (mkName phantomName) [PlainTV (mkName "a")]
            -- the constructor is only here to work around
            -- GHC sourceforge bug #1244882.
            [return $ NormalC (mkName (phantomName ++ "dummy")) []] 
            [],
        
        -- type $(name) a = $(super) ($(phantomName) a)
        tySynD (mkName name) [PlainTV (mkName "a")]
            (conT (mkName super) `appT` (conT (mkName phantomName)
                                        `appT` varT (mkName "a"))),
        
        -- type $(metaClassName) a = $(superMetaClassName) ($(phantomName) a)
        tySynD (mkName metaClassName) [PlainTV (mkName "a")]
            (conT (mkName superMetaClassName)
                `appT` (conT (mkName phantomName)
                        `appT` varT (mkName "a"))),

        -- type $(metaMetaClassName) a = $(superMetaMetaClassName) ($(phantomName) a)
        tySynD (mkName metaMetaClassName) [PlainTV (mkName "a")]
            (conT (mkName superMetaMetaClassName)
                `appT` (conT (mkName phantomName)
                        `appT` varT (mkName "a"))),

        -- $(classObjectName) :: $(metaClassName) ()
        sigD (mkName classObjectName) (conT (mkName metaClassName)
                                            `appT` [t| () |]),
                
        -- $(classObjectName) = unsafeGetClassObject "name"
        valD (return $ VarP (mkName classObjectName))
            (normalB [| unsafeGetClassObject $(stringE name) |]) [],         

        -- $(superName) :: String
        sigD (mkName superName) [t| String |],

        -- $(superName) = "super"
        valD (return $ VarP (mkName superName)) (normalB $ stringE super) [],
        
        -- instance SuperClass (name ()) (super ())
        instanceD (cxt []) (conT ''SuperClass `appT` clsType `appT` superType) [],
        
        -- instance SuperClass (clsName ()) (superClsName ())
        instanceD (cxt []) (conT ''SuperClass `appT` metaClsType `appT` superMetaType) [],
        
        -- instance ClassObject (metaClsName ())
        --  where classObject = classObject
        instanceD (cxt []) (conT ''ClassObject `appT` metaClsType)
            [funD 'classObject [clause [] (normalB $ varE (mkName classObjectName)) []]],

        -- instance RawStaticClass (name ()) where
        --  rawStaticClassForObject _ = unsafeGetRawClassObject "name"
        instanceD (cxt []) (conT ''RawStaticClass `appT` clsType)
            [funD 'rawStaticClassForObject [
                clause [wildP] (normalB $ 
                [| unsafeGetRawClassObject $(stringE name) |] ) []]]
    ]
    where
        phantomName = name ++ "_"
        metaClassName = name ++ "Class"
        metaMetaClassName = name ++ "MetaClass"
        metaPhantomName = metaClassName ++ "_"
        superMetaClassName | super == "ID" = "Class"
                           | otherwise = super ++ "Class"
        superMetaMetaClassName | super == "ID" = "MetaClass"
                               | otherwise = super ++ "MetaClass"
        classObjectName = "_" ++ name
        
        superName = "super_" ++ name

        metaMetaClsType = conT (mkName metaMetaClassName) `appT` [t| () |]
        metaClsType = conT (mkName metaClassName) `appT` [t| () |]
        clsType   = conT (mkName name)  `appT` [t| () |]
        superType = conT (mkName super) `appT` [t| () |]
        superMetaType = conT (mkName superMetaClassName) `appT` [t| () |]
