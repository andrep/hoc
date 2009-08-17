{-# LANGUAGE TemplateHaskell, EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies,
             UndecidableInstances, ScopedTypeVariables #-}
module HOC.Arguments where

import HOC.FFICallInterface

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe(unsafePerformIO)

import HOC.TH

-- ObjCArgument is the type class for Objective C arguments.
-- exportArgument is the FFIType type used when exporting this type.
-- importArgument is the FFIType used when importing this type
-- objCTypeString is the type string that should be used to identify this type 
-- to the objective-c runtime.
class (Storable b, FFITypeable b) => ObjCArgument a b | a -> b where
    withExportedArgument :: a -> (b -> IO c) -> IO c
    exportArgument :: a -> IO b
    exportArgumentRetained :: a -> IO b
    importArgument :: b -> IO a
    
    objCTypeString :: a -> String
    
    withExportedArgument arg action = exportArgument arg >>= action
    
    exportArgumentRetained = exportArgument
{-
    For types that are Storable & FFITypeable, define
    
    instance ObjCArgument MyType MyType where
        withExportedArgument = flip ($)
        exportArgument = return
        importArgument = return 
-}

-- like withArray, only tanks the ObjCArgument a b, and uses the ObjCArgument 
-- to translate a to b before constructing the array in the IO monad.
-- b is typable to Ptr b.
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
                exportArgument x = return x
                importArgument x = return x
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

-- This is the objective c method implementation type class.
-- objCImpGetArgsFFI get the FFIType of the arguments (as an array in the IO 
-- monad)
-- objCImpGetRetFFI does the same thing for the return type
class ObjCIMPType a where
    objCImpGetArgsFFI :: a -> IO [FFIType]
    objCImpGetRetFFI :: a -> IO FFIType
    
    objCImpGetArgsString :: a -> [String]
    objCImpGetRetString :: a -> String

-- This defines a ObjCArgument as an objective-c method implementation.  This 
-- is so that constant expressions can be used as implementations.
instance ObjCArgument a b => ObjCIMPType (IO a) where
    objCImpGetArgsFFI _ = return []
    objCImpGetRetFFI _ = makeFFIType (undefined :: b)

    objCImpGetArgsString _ = []
    objCImpGetRetString _ = objCTypeString (undefined :: a)

-- of course, a function, with the right types, is also an implementation type.
-- between this and the constant expression above, this will recursivly define
-- rank-n functions as ObjcIMPTypes (neat eh?)
instance (ObjCArgument a c, ObjCIMPType b) => ObjCIMPType (a -> b) where
    objCImpGetArgsFFI _ = do
        arg <- makeFFIType (undefined :: c)
        rest <- objCImpGetArgsFFI (undefined :: b)
        return (arg : rest)
    objCImpGetRetFFI _ = objCImpGetRetFFI (undefined :: b)
    
    objCImpGetArgsString _ = objCTypeString (undefined :: a)
                           : objCImpGetArgsString (undefined :: b)
    objCImpGetRetString _ = objCImpGetRetString (undefined :: b)

-- This creates the ffi_cif (or the haskell binding of it) for a given 
-- ObjCIMPType.  This is the heart of the glue for the language binding.
makeCifForSelector sel = do
    args <- objCImpGetArgsFFI sel
    ret <- objCImpGetRetFFI sel
    sel <- makeFFIType (undefined :: Ptr ())
    let orderedArgs = (last args : sel : init args)
    ffiPrepCif ret orderedArgs

-- This creates the ffi_cif for a given function.
makeCifForFunction fun = do
    args <- objCImpGetArgsFFI fun
    ret <- objCImpGetRetFFI fun
    ffiPrepCif ret args

{-# NOINLINE getCifForSelector #-} -- might be called from generated code
getCifForSelector sel = unsafePerformIO $ makeCifForSelector sel

{-# NOINLINE getCifForFunction #-} -- might be called from generated code
getCifForFunction fun = unsafePerformIO $ makeCifForFunction fun

-- This creates an objective-c type signature for a given ObjCIMPType
objCMethodType thing = ret ++ concat (last args : ":" : init args)
    where
        args = objCImpGetArgsString thing
        ret = objCImpGetRetString thing
