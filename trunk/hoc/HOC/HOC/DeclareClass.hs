module HOC.DeclareClass where

import HOC.Base
import HOC.Arguments
import HOC.Class

import Language.Haskell.THSyntax
import Foreign.Ptr

declareClass :: String -> String -> Q [Dec]

declareClass name super = sequence $ [
        -- data $(phantomName) a
        dataD (cxt []) phantomName ["a"] [] [],
        
        -- type $(name) a = $(super) ($(phantomName) a)
        tySynD name ["a"] (conT super `appT` (conT phantomName `appT` varT "a")),
        
        -- type $(metaClassName) a = $(superMetaClassName) ($(phantomName) a)
        tySynD metaClassName ["a"] (conT superMetaClassName `appT` (conT phantomName `appT` varT "a")),

        -- $(classObjectName) :: $(metaClassName) ()
        sigD classObjectName (conT metaClassName `appT` [t| () |]),
                
        -- $(classObjectName) = unsafeGetClassObject "name"
        valD (VarP classObjectName) (normalB [| unsafeGetClassObject $(stringE name) |]) [],         
    
        -- $(superName) = "super"
        valD (VarP superName) (normalB [| super |]) []
    ]
    where
        phantomName = name ++ "_"
        metaClassName = name ++ "Class"
        metaPhantomName = metaClassName ++ "_"
        superMetaClassName | super == "ID" = "Class"
                           | otherwise = super ++ "Class"
        classObjectName = "_" ++ name
        superName = "super_" ++ name
