module HOC.NewClass(
        IMP,
        MethodList,
        IvarList,
        newClass,
        makeMethodList,
        makeIvarList,
        setIvarInList,
        setMethodInList,
        makeDefaultIvarList,
        defaultIvarSize,
        setHaskellRetainMethod,
        setHaskellReleaseMethod,
        setHaskellDataMethod
    ) where

import HOC.Base
import HOC.ID
import HOC.FFICallInterface
import HOC.Arguments
import HOC.Class

import Foreign.C.String
import Foreign

type IMP = FFICif -> Ptr () -> Ptr (Ptr ()) -> IO (Ptr ObjCObject)
foreign import ccall "wrapper" wrapIMP :: IMP -> IO (FunPtr IMP)

newtype MethodList = MethodList (Ptr MethodList)
newtype IvarList = IvarList (Ptr IvarList)

foreign import ccall "NewClass.h newClass"
    newClass :: Ptr ObjCObject -> CString
             -> Int -> IvarList
             -> MethodList -> MethodList
             -> IO ()

foreign import ccall "NewClass.h makeMethodList"
    makeMethodList :: Int -> IO MethodList
foreign import ccall "NewClass.h setMethodInList"
    rawSetMethodInList :: MethodList -> Int
                    -> SEL -> CString
                    -> FFICif -> FunPtr IMP
                    -> IO ()

                      
foreign import ccall "NewClass.h makeIvarList"
    makeIvarList :: Int -> IO IvarList
foreign import ccall "NewClass.h setIvarInList"
    setIvarInList :: IvarList -> Int
                  -> CString -> CString -> Int -> IO ()

setMethodInList methodList idx sel typ cif imp = do
    typC <- newCString typ
    thunk <- wrapIMP imp
    rawSetMethodInList methodList idx sel typC cif thunk

makeDefaultIvarList = do
    list <- makeIvarList 1
    name <- newCString "__retained_haskell_part__"
    typ <- newCString "^v"
    setIvarInList list 0 name typ 0
    return list

defaultIvarSize = 4 :: Int

retainSelector = getSelectorForName "retain"
retainCif = getCifForSelector (undefined :: ID () -> IO (ID ()))

releaseSelector = getSelectorForName "release"
releaseCif = getCifForSelector (undefined :: ID () -> IO ())

getHaskellDataSelector = getSelectorForName "__getHaskellData__"
getHaskellDataCif = getCifForSelector (undefined :: Class () -> ID () -> IO (ID ()))
                                                -- actually  -> IO (Ptr ()) ...

setHaskellRetainMethod methodList idx = do
    typC <- newCString "@@:"
    thunk <- wrapIMP haskellObject_retain_IMP
    rawSetMethodInList methodList
                       idx
                       retainSelector
                       typC
                       retainCif
                       thunk
    
setHaskellReleaseMethod methodList idx = do
    typC <- newCString "v@:"
    thunk <- wrapIMP haskellObject_release_IMP
    rawSetMethodInList methodList
                       idx
                       releaseSelector
                       typC
                       releaseCif
                       thunk

setHaskellDataMethod methodList idx super mbDat = do
    typC <- newCString "^v@:#"
    thunk <- wrapIMP (getHaskellData_IMP super mbDat)
    rawSetMethodInList methodList
                       idx
                       getHaskellDataSelector
                       typC
                       getHaskellDataCif
                       thunk

