{-# LANGUAGE TemplateHaskell #-}
module HOC.DeclareClass(declareClass) where

import HOC.Base
import HOC.Arguments
import HOC.Class
import HOC.Super

import HOC.TH

import Foreign.Ptr

declareClass :: String -> String -> Q [Dec]

declareClass name super = sequence $ [
        -- data $(phantomName) a
        dataD (cxt []) (mkName phantomName) [mkName "a"]
            -- the constructor is only here to work around
            -- GHC sourceforge bug #1244882.
            [return $ NormalC (mkName (phantomName ++ "dummy")) []] 
            [],
        
        -- type $(name) a = $(super) ($(phantomName) a)
        tySynD (mkName name) [mkName "a"]
            (conT (mkName super) `appT` (conT (mkName phantomName)
                                        `appT` varT (mkName "a"))),
        
        -- type $(metaClassName) a = $(superMetaClassName) ($(phantomName) a)
        tySynD (mkName metaClassName) [mkName "a"]
            (conT (mkName superMetaClassName)
                `appT` (conT (mkName phantomName)
                        `appT` varT (mkName "a"))),

        -- type $(metaMetaClassName) a = $(superMetaMetaClassName) ($(phantomName) a)
        tySynD (mkName metaMetaClassName) [mkName "a"]
            (conT (mkName superMetaMetaClassName)
                `appT` (conT (mkName phantomName)
                        `appT` varT (mkName "a"))),

        -- $(classObjectName) :: $(metaClassName) ()
        sigD (mkName classObjectName) (conT (mkName metaClassName)
                                            `appT` [t| () |]),
                
        -- $(classObjectName) = unsafeGetClassObject "name"
        valD (return $ VarP (mkName classObjectName))
            (normalB [| unsafeGetClassObject $(stringE name) |]) [],         

        -- $(metaClassObjectName) = unsafeGetMetaclassForClass $(classObjectName)
        valD (return $ VarP (mkName metaClassObjectName))
            (normalB [| unsafeGetMetaclassForClass $(varE (mkName classObjectName)) |]) [],         

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
        
        -- instance ClassObject metaMetaCls
        --  where classObject = unsafeGetMetaclassForClass classObject
        --  {- metaclass object, to support super calls in class methods -}
        instanceD (cxt []) (conT ''ClassObject `appT` metaMetaClsType)
            [funD 'classObject [clause [] (normalB $ varE (mkName metaClassObjectName)) []]]
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
        metaClassObjectName = "_" ++ metaClassName
        superName = "super_" ++ name

        metaMetaClsType = conT (mkName metaMetaClassName) `appT` [t| () |]
        metaClsType = conT (mkName metaClassName) `appT` [t| () |]
        clsType   = conT (mkName name)  `appT` [t| () |]
        superType = conT (mkName super) `appT` [t| () |]
        superMetaType = conT (mkName superMetaClassName) `appT` [t| () |]
