{-# OPTIONS -fallow-undecidable-instances #-}
module HOC.Arguments where

import HOC.Base
import HOC.FFICallInterface

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe(unsafePerformIO)

import HOC.TH

class (Storable b, FFITypeable b) => ObjCArgument a b | a -> b where
    withExportedArgument :: a -> (b -> IO c) -> IO c
    exportArgument :: a -> IO b
    importArgument :: b -> IO a
    
    objCTypeString :: a -> String
    
    withExportedArgument arg action = exportArgument arg >>= action
{-
    For types that are Storable & FFITypeable, define
    
    instance ObjCArgument MyType MyType where
        withExportedArgument = flip ($)
        exportArgument = return
        importArgument = return 
-}

withExportedArray :: ObjCArgument a b => [a] -> (Ptr b -> IO c) -> IO c
withExportedArray l a = withExportedList l $ \l' -> withArray l' a
    where
        withExportedList [] a = a []
        withExportedList (x:xs) a
            = withExportedArgument x $
              \x' -> withExportedList xs $
              \xs' -> a (x':xs')

declareStorableObjCArgument :: TypeQ -> String -> Q [Dec]

{- This is what we'd like to do.
declareStorableObjCArgument ty str =
    [d| instance ObjCArgument $(ty) $(ty) where
            exportArgument = return
            importArgument = return
            objCTypeString = str
    |]
-}

declareStorableObjCArgument ty str = do
    argInst <- instanceD (cxt []) (conT ''ObjCArgument
        `appT` ty `appT` ty)
            `whereQ` [d|
                {- withExportedArgument = flip ($) -}
                exportArgument = return
                importArgument = return 
                objCTypeString _ = str
            |]
    return [argInst]

    -- to avoid overlapping instance ObjCIMPType (IO ()) below...
data EvilDummyForUnit
instance Storable EvilDummyForUnit where
    sizeOf = undefined ; alignment = undefined ; peek = undefined ; poke = undefined
instance FFITypeable EvilDummyForUnit where
    makeFFIType _ = makeFFIType ()
instance ObjCArgument () EvilDummyForUnit where
    exportArgument = undefined
    importArgument = undefined
    objCTypeString _ = "v"

class ObjCIMPType a where
    objCImpGetArgsFFI :: a -> IO [FFIType]
    objCImpGetRetFFI :: a -> IO FFIType
    
    objCImpGetArgsString :: a -> [String]
    objCImpGetRetString :: a -> String
    
instance ObjCArgument a b => ObjCIMPType (IO a) where
    objCImpGetArgsFFI _ = return []
    objCImpGetRetFFI _ = makeFFIType (undefined :: b)

    objCImpGetArgsString _ = []
    objCImpGetRetString _ = objCTypeString (undefined :: a)

instance (ObjCArgument a c, ObjCIMPType b) => ObjCIMPType (a -> b) where
    objCImpGetArgsFFI _ = do
        arg <- makeFFIType (undefined :: c)
        rest <- objCImpGetArgsFFI (undefined :: b)
        return (arg : rest)
    objCImpGetRetFFI _ = objCImpGetRetFFI (undefined :: b)
    
    objCImpGetArgsString _ = objCTypeString (undefined :: a)
                           : objCImpGetArgsString (undefined :: b)
    objCImpGetRetString _ = objCImpGetRetString (undefined :: b)

makeCifForSelector sel = do
    args <- objCImpGetArgsFFI sel
    ret <- objCImpGetRetFFI sel
    sel <- makeFFIType (undefined :: Ptr ())
    let orderedArgs = (last args : sel : init args)
    ffiPrepCif ret orderedArgs

makeCifForFunction fun = do
    args <- objCImpGetArgsFFI fun
    ret <- objCImpGetRetFFI fun
    ffiPrepCif ret args

{-# NOINLINE getCifForSelector #-} -- might be called from generated code
getCifForSelector sel = unsafePerformIO $ makeCifForSelector sel

{-# NOINLINE getCifForFunction #-} -- might be called from generated code
getCifForFunction fun = unsafePerformIO $ makeCifForFunction fun

    
objCMethodType thing = ret ++ concat (last args : ":" : init args)
    where
        args = objCImpGetArgsString thing
        ret = objCImpGetRetString thing
