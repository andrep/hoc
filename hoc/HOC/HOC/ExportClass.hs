{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module HOC.ExportClass where

import Foreign.C.String
import Control.Concurrent.MVar
import Data.Dynamic
import Data.Typeable
import Data.Maybe(mapMaybe)
import Data.Char(toUpper)

import HOC.Base
import HOC.MessageTarget
import HOC.Arguments
import HOC.Invocation
import HOC.ID
import HOC.SelectorMarshaller
import HOC.Class
import HOC.NewClass
import HOC.TH
import HOC.Exception

data ClassMember =
        InstanceMethod Name
    |   ClassMethod Name
    |   Outlet String TypeQ
    |   InstanceVariable String TypeQ ExpQ

class (Object cls, Typeable ivars) => InstanceVariables cls ivars
      | cls -> ivars, ivars -> cls
      where
    initializeInstanceVariables :: IO ivars

data IVar i v = IVar (i -> MVar v)

getAndSetIVar :: InstanceVariables cls iv => IVar iv a -> a -> cls -> IO a
setIVar :: InstanceVariables cls iv => IVar iv a -> a -> cls -> IO ()
getIVar :: InstanceVariables cls iv => IVar iv a -> cls -> IO a
getInstanceMVar :: InstanceVariables cls iv => IVar iv a -> cls -> MVar a

getInstanceVariablesForObject :: InstanceVariables cls iv => cls -> iv
getInstanceVariablesForObject obj = head $ mapMaybe fromDynamic $ getHaskellDataForID  $ toID obj

getInstanceMVar (IVar extract) obj = extract $ getInstanceVariablesForObject obj

setIVar ivar val obj = modifyMVar_ (getInstanceMVar ivar obj) (\_ -> return val)
getAndSetIVar ivar val obj = swapMVar (getInstanceMVar ivar obj) val
getIVar ivar obj = readMVar (getInstanceMVar ivar obj)

getAsID :: InstanceVariables cls iv => IVar iv (ID a) -> cls -> IO (ID ())
getAsID ivar obj = getIVar ivar obj >>= return . toID

setAsID :: InstanceVariables cls iv => IVar iv (ID a) -> ID () -> cls -> IO ()
setAsID ivar val obj = setIVar ivar (fromID val) obj

type SetVarImpType target inst = ID () -> target -> IO ()
type GetVarImpType target inst = target -> IO (ID ())
setVarCif = getCifForSelector (undefined :: SetVarImpType (ID ()) (ID ()))
getVarCif = getCifForSelector (undefined :: GetVarImpType (ID ()) (ID ()))


exportClass :: String -- ^ Name of class you're exporting, e.g. "MyDocument"
            -> String -- ^ A prefix for function names which are methods 
                      --   belonging to this class, e.g. "md_"
            -> [ClassMember] -- ^ A list of class members, such as outlets
                             --   and instance variables
            -> Q [Dec] -- ^ A Haskell declaration, which can be spliced in
                       --   with Template Haskell's $(...) syntax
exportClass name prefix members = sequence $ [
        sigD (mkName exportFunName) [t| IO () |],
        valD (varP $ mkName exportFunName)
            (normalB (mkClassExportAction name prefix members)) [],
        dataD (cxt []) (mkName instanceDataName) []
            [normalC (mkName instanceDataName) strictTypes] [''Typeable],
        valD (varP $ mkName tyConVar) (normalB [| mkTyCon instanceDataName |]) [],
        instanceD (cxt []) (conT ''InstanceVariables
                            `appT` clsTy `appT` instTy) 
                [
--                  All we want to do is this:
--                      initializeInstanceVariables = $(initIVars)
--                  But we want the name initializeInstanceVariables refer directly
--                  to this module, so that we don't have to export it, but keep it
--                  private.
                    do e <- initIVars
                       return (ValD (VarP 'initializeInstanceVariables) (NormalB e) [])
                ]
    ] ++ declaredIVars
    where
        exportFunName = "initializeClass_" ++ name
        instanceDataName = name ++ "_IVARS"
        tyConVar = "tycon_" ++ name ++ "_IVARS"
        strictTypes = map (strictType (return IsStrict)) wrappedIvarTypes
        ivars = [ (name, ty, [| nil |]) | Outlet name ty <- members ]
             ++ [ (name, ty, initial)   | InstanceVariable name ty initial <- members ]
        wrappedIvarTypes = [ conT ''MVar `appT` ty | (_,ty,_) <- ivars ]
        ivarNames = [ name | (name,_,_) <- ivars ]
        clsTy = conT (mkName name) `appT` [t| () |]
        instTy = conT (mkName instanceDataName)
        nIVars = length ivarNames
        
        declaredIVars = concat $ zipWith declareIVar ivars [1..]
        declareIVar (name, ty, _) n = [
            sigD (mkName ('_':name))
              (conT ''IVar `appT` (conT $ mkName instanceDataName) `appT` ty)
          , valD (varP $ mkName ('_':name))
              (normalB [| IVar $(getNth n) |]) []
          ]
            where
                getNth n = lamE [conP (mkName instanceDataName) (args n)]
                                (varE $ mkName $ "arg" ++ show n)
                args n = [ mkVarP (n == i) $ "arg" ++ show i | i <- [1..nIVars] ]
        
        initIVars = doE (map initIVar ivars ++ [noBindS [| return $(wrap) |]])
            where
                wrap = foldl appE (conE $ mkName instanceDataName) (map (varE.mkName) ivarNames)
                initIVar (ivar,ty,initial) = bindS (varP $ mkName ivar) [| newMVar $(initial) |]

-- | Declare a variable (with a preceeding _ unless it is used)
mkVarP :: Bool -> String -> PatQ
mkVarP used = varP . mkName . (if used then id else ('_':))

data Method = ImplementedMethod Name
            | GetterMethod String
            | SetterMethod String

mkClassExportAction name prefix members =
        [| 
            do
                super <- getClassByName $(varE $ mkName $ "super_" ++ name)
                ivars <- makeDefaultIvarList
                imethods <- makeMethodList (nIMethods+3)
                cmethods <- makeMethodList nCMethods
                setHaskellRetainMethod  imethods 0 super
                setHaskellReleaseMethod imethods 1 super
                setHaskellDataMethod imethods 2 super (
                        Just ($(typedInitIvars) >>= return . toDyn)
                    )
                $(fillMethodList False 3 [|imethods|] instanceMethods) 
                $(fillMethodList True 0 [|cmethods|] classMethods)
                clsname <- newCString name
                newClass super clsname ivars imethods cmethods
        |]
    where
        typedInitIvars = [|initializeInstanceVariables|]
            `sigE` (conT ''IO `appT` conT (mkName $ name ++ "_IVARS"))
    
        outlets = [ name | Outlet name _ <- members ]
        classMethods = [ ImplementedMethod n | ClassMethod n <- members ] 

        explicitInstanceMethods = [ n | InstanceMethod n <- members ]         
        instanceMethodNames = map nameBase explicitInstanceMethods
        instanceMethods =
                map ImplementedMethod explicitInstanceMethods 
             ++ [ GetterMethod ivar | ivar <- outlets,
                                      not (ivar `elem` instanceMethodNames) ]
             ++ [ SetterMethod ivar | ivar <- outlets,
                                      not (setterNameForH ivar
                                           `elem` instanceMethodNames) ]
  
        nIMethods = length instanceMethods
        nCMethods = length classMethods
        
            -- GHC fails with Prelude.last if we pass it an empty doE
        fillMethodList isClassMethod firstIdx objCMethodList [] = [| return () |]  
        fillMethodList isClassMethod firstIdx objCMethodList methods =
            doE $
            map (noBindS . exportMethod isClassMethod objCMethodList)
                (zip methods [firstIdx..])
                
        exportMethod isClassMethod objCMethodList
                     (ImplementedMethod selName, num)
            = do
                VarI _ t _ _ <- reify $ selName
                let arrowsToList (AppT (AppT ArrowT a) b)
                        = a : arrowsToList b
                    arrowsToList (AppT (ConT c) b)
                        | c == ''IO
                        = [b]
                    arrowsToList (ForallT _ _ a)
                        = arrowsToList a
                    ts = arrowsToList t
                
                    nArgs = length ts - 2  -- subtract target and result
                    isUnit = last ts == ConT ''()
                
                exportMethod' isClassMethod objCMethodList num methodBody
                              nArgs isUnit impTypeName selExpr cifExpr
                              retainedExpr
            where
                methodBody = varE $ mkName $ prefix ++ nameBase selName
                
                -- selName = selectorInfoHaskellName selectorInfo
                
                impTypeName = ("ImpType_" ++ nameBase selName)
                                `fromSameModuleAs_tc` selName
                infoName = ("info_" ++ nameBase selName)
                            `fromSameModuleAs_v` selName
                
                selExpr = [| selectorInfoSel $(varE $ infoName) |]
                cifExpr = [| selectorInfoCif $(varE $ infoName) |]
                retainedExpr = [| selectorInfoResultRetained $(varE $ infoName) |]
        
        exportMethod isClassMethod objCMethodList (GetterMethod ivarName, num) =
                exportMethod' isClassMethod objCMethodList num
                              ([| getAsID |] `appE` varE (mkName ('_':ivarName)))
                              0 False (''GetVarImpType)
                              [| getSelectorForName ivarName |]
                              [| getVarCif |]
                              [| False |]
            
        exportMethod isClassMethod objCMethodList (SetterMethod ivarName, num) =
                exportMethod' isClassMethod objCMethodList num
                              ([| setAsID |] `appE` varE (mkName ('_':ivarName)))
                              1 True (''SetVarImpType)
                              [| getSelectorForName setterName |]
                              [| setVarCif |]
                              [| False |]
            where
                setterName = setterNameFor ivarName
        
        setterNameFor ivarName = setterNameForH ivarName ++ ":"
        setterNameForH ivarName = "set" ++ toUpper (head ivarName) : tail ivarName
            
                
        exportMethod' isClassMethod objCMethodList num methodBody
                       nArgs isUnit impTypeName selExpr cifExpr retainedExpr =
            [|
                setMethodInList $(objCMethodList)
                                num
                                $(selExpr)
                                (objCMethodType $(typed [|undefined|]))
                                $(cifExpr)
                                ($(lamE (map (uncurry mkVarP) [(False,"cif"),(not isUnit,"ret"),(True,"args")]) marshal))
            |]
            where
                marshal = [| do recordHOCEvent kHOCEnteredHaskell $(varE $ mkName "args")
                                exc <- exceptionHaskellToObjC $(marshal')
                                recordHOCEvent kHOCAboutToLeaveHaskell $(varE $ mkName "args")
                                return exc
                          |]
                marshal' = doE $ getArg ("slf",0)
                                 : map getArg (zip arguments [2..])
                                 ++ [
                                    noBindS [| recordHOCEvent kHOCImportedArguments $(varE $ mkName "args") |],
                                    noBindS invokeAndReturn
                                 ]
                
                
                arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
                
                invokeAndReturn
                    | isUnit =
                        typedBodyWithArgs
                    | otherwise =
                        [| do result <- $(typedBodyWithArgs)
                              recordHOCEvent kHOCAboutToExportResult $(varE $ mkName "args")
                              setMarshalledRetval $(retainedExpr) $(varE $ mkName "ret") result
                        |]
                
                typedBodyWithArgs = foldl1 appE (typed methodBody
                                                : map (varE.mkName)(arguments ++ ["slf"])) 
                    where
                        arguments = ["arg" ++ show i | i <- [1..nArgs]]
                
                typed thing = thing
                            `sigE` (conT impTypeName
                                    `appT` (targetType
                                            `appT` [t| () |])
                                    `appT` (instanceType
                                            `appT` [t| () |])
                                     )
                
                targetType   | isClassMethod = conT $ mkName $ name ++ "Class"
                             | otherwise     = instanceType
                             
                instanceType = conT $ mkName name
                
                getArg (argname, argnum) =
                    bindS (varP (mkName argname))
                          [| getMarshalledArgument $(varE $ mkName "args") argnum |]
