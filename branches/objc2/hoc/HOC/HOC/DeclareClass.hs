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

        -- $(classObjectName) :: $(metaClassName) ()
        sigD (mkName classObjectName) (conT (mkName metaClassName)
                                            `appT` [t| () |]),
                
        -- $(classObjectName) = unsafeGetClassObject "name"
        valD (return $ VarP (mkName classObjectName))
            (normalB [| unsafeGetClassObject $(stringE name) |]) [],         

        sigD (mkName superName) [t| String |],

        -- $(superName) = "super"
        valD (return $ VarP (mkName superName)) (normalB $ stringE super) [],
        
        -- instance SuperClass (name ()) (super ())
        instanceD (cxt []) (conT ''SuperClass `appT` clsType `appT` superType) [],
        
        -- instance ClassObject (metaClsName ())
        --  where classObject = classObject
        instanceD (cxt []) (conT ''ClassObject `appT` metaClsType)
            [funD 'classObject [clause [] (normalB $ varE (mkName classObjectName)) []]]
    ]
    where
        phantomName = name ++ "_"
        metaClassName = name ++ "Class"
        metaPhantomName = metaClassName ++ "_"
        superMetaClassName | super == "ID" = "Class"
                           | otherwise = super ++ "Class"
        classObjectName = "_" ++ name
        superName = "super_" ++ name

        metaClsType = conT (mkName metaClassName) `appT` [t| () |]
        clsType   = conT (mkName name)  `appT` [t| () |]
        superType = conT (mkName super) `appT` [t| () |]
