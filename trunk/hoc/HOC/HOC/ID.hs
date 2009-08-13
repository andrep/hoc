{-# LANGUAGE ForeignFunctionInterface, RecursiveDo,
             MultiParamTypeClasses, FlexibleInstances #-}
module HOC.ID where

import HOC.Base
import HOC.Arguments
import HOC.FFICallInterface(FFICif)

import Control.Concurrent.MVar
import Control.Exception(evaluate,assert)
import Control.Monad(when, join)
import System.IO.Unsafe(unsafePerformIO)
import System.Mem.Weak
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types(CInt,CUInt,CChar {- ObjC BOOL is typedefed to char -})
import Foreign.Storable
import Foreign.Marshal.Alloc(alloca)
import Data.Dynamic
import Data.Maybe(fromMaybe, isJust)

data ID a = ID HSO | Nil

dPutStrLn = if {--} False --} True
    then putStrLn
    else const $ return ()

dPutWords = dPutStrLn . unwords

nil = Nil

castObject (ID a) = ID a
castObject Nil = Nil

instance Eq (ID a) where
    (ID (HSO a _)) == (ID (HSO b _))    = a == b
    Nil == Nil                          = True
    _ == _                              = False


{-
    *
    * WARNING: Arcane Magic Follows
    *
    
    The code below is about importing and exporting IDs to/from Haskell land.
    Most importantly, there's an instance for ObjCArgument (ID a) (Ptr ObjCObject).
    
    Every Object that is currently referenced from Haskell has exactly
    one "HaskellSideObject"; a weak pointer to this is kept in a global
    NSMapTable (see ObjectMapC.m).
    
    Additionally, objects that are implemented (at least partially) in Haskell
    get a stable pointer from the Objective C part of the object to the Haskell part,
    but only as long as there are references from Objective C code to that object.
    This keeps the Haskell part of the object (which contains Haskell-defined
    instance vars) alive even if there are no references to the object from Haskell
    land.
-}

-- HSO: HaskellSideObject
data HSO = HSO (Ptr ObjCObject) [Dynamic]

-- don't we love globals?  This needs -fno-cse to be truely safe.
objectMapLock = unsafePerformIO $ newMVar ()
{-# NOINLINE objectMapLock #-}

withObjectMapLock taker action = do
    dPutWords [">", "withObjectMapLock", taker]
    res <- withMVar objectMapLock $ \_ -> action
    dPutWords ["<", "withObjectMapLock", taker]
    return res


-- given a pointer to an ObjCObject, return a stablePtr to a Weak reference to 
-- a HSO
foreign import ccall unsafe "ObjectMap.h getHaskellPart"
    getHaskellPart :: Ptr ObjCObject -> IO (StablePtr (Weak HSO))
-- Sets the mapping of an ObjCObject to the HSO.  The CInt is a boolean flag 
-- used to set whether or not this objective-c object is immortal.  The is 
-- equivelent to saying removeHaskellPart will never be called for this object
foreign import ccall unsafe "ObjectMap.h setHaskellPart"
    setHaskellPart :: Ptr ObjCObject -> StablePtr (Weak HSO) -> CInt -> IO ()
-- remove the objcobject->HSO mapping.  You have to pass both because this 
-- method does nothing if the objCObject maps to a different HSO.  This can 
-- happen if the finalizer ran late and the object was reimported in the 
-- meantime.
foreign import ccall unsafe "ObjectMap.h removeHaskellPart"
    removeHaskellPart :: Ptr ObjCObject -> StablePtr (Weak HSO) -> IO ()
-- returns number of objects allocated in the map and the immortal count in the 
-- two pointers.
foreign import ccall unsafe "ObjectMap.h objectMapStatistics"
    c_objectMapStatistics :: Ptr CUInt -> Ptr CUInt -> IO ()

-- must be "safe", because it calls methods implemented in Haskell.
foreign import ccall safe "GetNewHaskellData.h getNewHaskellData"
    getNewHaskellData :: Ptr ObjCObject -> IO (StablePtr ([Dynamic]))
foreign import ccall safe "GetNewHaskellData.h getNewHaskellDataForClass"
    getNewHaskellDataForClass :: Ptr ObjCObject
                              -> Ptr ObjCObject
                              -> IO (StablePtr ([Dynamic]))

foreign import ccall unsafe "RetainedHaskellPart.h getRetainedHaskellPart"
    getRetainedHaskellPart :: Ptr ObjCObject -> IO (StablePtr HSO)
foreign import ccall unsafe "RetainedHaskellPart.h setRetainedHaskellPart"
    setRetainedHaskellPart :: Ptr ObjCObject -> StablePtr HSO -> IO ()
replaceRetainedHaskellPart :: Ptr ObjCObject -> StablePtr HSO -> IO ()
replaceRetainedHaskellPart self newHSO = do
    dPutWords ["replaceRetainedHaskellPart", show self, show (castStablePtrToPtr newHSO)]
    oldHSO <- getRetainedHaskellPart self
    when (oldHSO /= newHSO) $ do
        when (castStablePtrToPtr oldHSO /= nullPtr) $ do
            freeStablePtr oldHSO
        setRetainedHaskellPart self newHSO

foreign import ccall "MemoryManagement.h retainSuper"
    retainSuper :: Ptr ObjCObject -> Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h releaseSuper"
    releaseSuper :: Ptr ObjCObject -> Ptr ObjCObject -> IO ()
foreign import ccall unsafe "MemoryManagement.h retainCount"
    retainCount :: Ptr ObjCObject -> IO CUInt

-- Since finalizers are executed in arbitrary threads, we must
-- ensure that we establish an autoreleasepool for the duration
-- of the execution of the release/dealloc messages.
-- Since the execution of each finalizer might get spread out
-- over several native threads, we perform the operation
-- together with pool allocation in c, to avoid allocating the
-- pool in one thread, executing the release/dealloc in a second
-- and freeing the pool in a third.
foreign import ccall "MemoryManagement.h releaseObjectWithPool"
    releaseObjectWithPool :: Ptr ObjCObject -> IO ()
foreign import ccall "MemoryManagement.h deallocObjectWithPool"
    deallocObjectWithPool :: Ptr ObjCObject -> IO ()



instance ObjCArgument (ID a) (Ptr ObjCObject) where
    -- remember that thing may be lazy and never evaluated,
    -- including by "action"  Thus you must evaluate "thing"
    -- to ensure that the HSO object is properly allocated.
    withExportedArgument (ID thing@(HSO arg _)) action = do
        result <- action arg
        evaluate thing  -- make sure it's still alive
        return result
    withExportedArgument Nil action = action (nullPtr)
    
    -- Again, as above, only this time, we need to make sure the object is 
    -- retained long enough to be useful as an argument.
    exportArgument (ID thing@(HSO arg _)) = do
        retainObject arg
        evaluate thing  -- make sure the HSO has been alive until now
        autoreleaseObject arg
        return arg
    exportArgument Nil = return nullPtr
   
    -- this time with no autorelease.  This method effectively claims
    -- ownership of the object.
    exportArgumentRetained (ID thing@(HSO arg _)) = do
        retainObject arg
        evaluate thing  -- make sure the HSO has been alive until now
        return arg
    exportArgumentRetained Nil = return nullPtr
    
    importArgument = importArgument' False
    
    objCTypeString _ = "@"

importImmortal = importArgument' True

-- this is where the magic happens.
importArgument' immortal p
    | p == nullPtr = return Nil
    | otherwise = do
        (haskellObj, retain) <- withObjectMapLock "importArgument'" $ do
            mbHaskellObj <- lookupHSO p
            case mbHaskellObj of
                -- if the HSO already exists, we're done!
                Just haskellObj -> return (haskellObj, False)
                -- otherwise create one and (outside the lock) retain p
                Nothing -> do
                    haskellObj <- makeNewHSO immortal p
                    return (haskellObj, True)
        when retain (retainObject p)
        return (ID haskellObj)

lookupHSO p = do
    sptr <- getHaskellPart p
    if castStablePtrToPtr sptr /= nullPtr
        then do
            wptr <- deRefStablePtr sptr
            deRefWeak wptr
        else
            return Nothing

-- notice that wptr's finalizer definition requires new_sptr, which
-- cannot be created till after the wptr;
-- so we use 'mdo' (it's much more pratical than fixM)
makeNewHSO immortal p = mdo
    haskellData <- makeNewHaskellData p
    dPutWords ["got haskell data", show haskellData]
    let haskellObj = HSO p (fromMaybe [] haskellData)
        finalizer | immortal = Nothing
                  | otherwise = Just $ finalizeID p new_sptr
    wptr <- mkWeakPtr haskellObj finalizer
    new_sptr <- newStablePtr wptr
    setHaskellPart p new_sptr (if immortal then 1 else 0)
    return haskellObj

finalizeID :: Ptr ObjCObject -> StablePtr (Weak HSO) -> IO ()
finalizeID cObj sptr = do
    withObjectMapLock "finalizeID" $ removeHaskellPart cObj sptr
    
    releaseObjectWithPool cObj
    freeStablePtr sptr

makeNewHaskellData p = do
    stable <- getNewHaskellData p
    if (castStablePtrToPtr stable == nullPtr)
        then return Nothing
        else do
            dat <- deRefStablePtr stable
            freeStablePtr stable
            return (Just dat)

haskellObject_retain_IMP :: Ptr ObjCObject -> FFICif -> Ptr () -> Ptr (Ptr ()) -> IO (Ptr ObjCObject)
haskellObject_retain_IMP super cif ret args = do
    selfPtr <- peekElemOff args 0
    self <- peek (castPtr selfPtr) :: IO (Ptr ObjCObject)
    poke (castPtr ret) self     -- retain returns self
    dPutWords ["haskellObject_retain_IMP", show super, "<FFICif>", show ret, show args]
    haskellObject_retain self super
    return nullPtr  -- no exception
    
haskellObject_retain self super = do
    dPutWords ["haskellObject_retain", show self, show super]
    retainSuper self super
    dPutStrLn "retained super"
    
    withObjectMapLock "haskellObject_retain" $ do
        stablePtrToHaskellSelf <- getRetainedHaskellPart self
        when (castStablePtrToPtr stablePtrToHaskellSelf == nullPtr) $ do
            stableWeakPtrToHaskellSelf <- getHaskellPart self
            when (castStablePtrToPtr stableWeakPtrToHaskellSelf /= nullPtr) $ do
                weakPtrToHaskellSelf <- deRefStablePtr stableWeakPtrToHaskellSelf
                mbHaskellSelf <- deRefWeak weakPtrToHaskellSelf
                case mbHaskellSelf of
                    Just haskellSelf -> do
                        stablePtrToHaskellSelf <- newStablePtr haskellSelf
                        setRetainedHaskellPart self stablePtrToHaskellSelf
                    Nothing ->
                        -- The weak pointer will only be dealloced when there are
                        -- no known references from ObjC and no references from Haskell.
                        -- So if we get here, it's not my bug (hopefully).
                        -- When an object is exported (returned or passed as a parameter)
                        -- from Haskell, it is retained and autoreleased, so passing an
                        -- object from Haskell to Objective C and immediately forgetting
                        -- the reference (before ObjC has a chance to retain it) is safe.
                
                        error "Error: Retaining Haskell Object that has already been released"


haskellObject_release_IMP :: Ptr ObjCObject -> FFICif -> Ptr () -> Ptr (Ptr ()) -> IO (Ptr ObjCObject)
haskellObject_release_IMP super cif ret args = do
    selfPtr <- peekElemOff args 0
    self <- peek (castPtr selfPtr) :: IO (Ptr ObjCObject)
    dPutWords ["haskellObject_release_IMP", show super, "<FFICif>", show ret, show args]
    haskellObject_release super self
    return nullPtr  -- no exception

haskellObject_release super self = do
    dPutWords ["haskellObject_release", show super, show self]
    retainCount+1 <- retainCount self
        -- retainCount+1 because we want to know the retainCount after we
        -- release; if it's about to become zero, then we won't be
        -- able to call retainCount on self after the call to releaseSuper.
    releaseSuper self super
        -- retainCount should now contain the current retain count.
    
    when (retainCount == 1) $ withObjectMapLock "haskellObject_release" $ do
        -- no extra references 
        -- Only the reference from the Haskell part remains,
        -- which means we do no longer want to have a stable pointer
        -- (if we have one, that is)
        replaceRetainedHaskellPart self (castPtrToStablePtr nullPtr)

-- this is the implementation of the __getHaskellData__ selector.
getHaskellData_IMP :: Ptr ObjCObject -> Maybe (IO Dynamic)
                    -> FFICif -> Ptr () -> Ptr (Ptr ()) -> IO (Ptr ObjCObject)
getHaskellData_IMP super mbDat cif ret args = do
    selfPtr <- peekElemOff args 0
    self <- peek (castPtr selfPtr) :: IO (Ptr ObjCObject)
    dPutWords ["__getHaskellData__", show self, show super]
    superDataStable <- getNewHaskellDataForClass self super
    superData <- if castStablePtrToPtr superDataStable == nullPtr
        then do
                return []
        else do
                dat <- deRefStablePtr superDataStable
                freeStablePtr superDataStable
                return dat
    
    datList <- case mbDat of
                    Just makeMyDat -> makeMyDat >>= return . (: superData)
                    Nothing -> return superData
    stable <- newStablePtr datList
    poke (castPtr ret) stable
    return nullPtr  -- no exception

getHaskellDataForID (ID (HSO _ dat)) = dat

releaseExtraReference obj
    = withExportedArgument obj (\ptr -> when (ptr /= nullPtr) (releaseObject ptr))
      >> return obj

objectMapStatistics =
    alloca $ \pAllocated ->
    alloca $ \pImmortal ->
    do
        c_objectMapStatistics pAllocated pImmortal
        allocated <- peek pAllocated
        immortal <- peek pImmortal
        return (allocated, immortal)
