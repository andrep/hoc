module HOC.ExportClass where

import Language.Haskell.THSyntax
import Foreign
import Foreign.C.String
import Control.Concurrent.MVar
import Data.Dynamic
import Data.Typeable
import Data.Maybe(mapMaybe)
import Data.Char(toUpper)

import HOC.Base
import HOC.ID
import HOC.Arguments
import HOC.Invocation
import HOC.SelectorMarshaller
import HOC.Class
import HOC.NewClass

data ClassMember =
        InstanceMethod SelectorInfo String
    |   ClassMethod SelectorInfo String
    |   Outlet String TypeQ
    |   InstanceVariable String TypeQ

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


exportClass :: String -> [ClassMember] -> Q [Dec]

exportClass name members = sequence $ [
        valD (VarP exportFunName) (normalB (mkClassExportAction name members)) [],
        dataD (cxt []) instanceDataName [] [normalC instanceDataName strictTypes] [],
        valD (VarP tyConVar) (normalB [| mkTyCon instanceDataName |]) [],
        instanceD (cxt []) (conT "Data.Typeable:Typeable" `appT` instTy) `whereQ`
            [d| typeOf _ = mkAppTy $(varE tyConVar) [] |],
        instanceD (cxt []) (conT (thModulePrefix "ExportClass" "InstanceVariables")
                            `appT` clsTy `appT` instTy) `whereQ`
            [d|
                initializeInstanceVariables = $(initIVars)
            |]
    ] ++ declaredIVars
    where
        exportFunName = "initializeClass_" ++ name
        instanceDataName = name ++ "_IVARS"
        tyConVar = "tycon_" ++ name ++ "_IVARS"
        strictTypes = map (strictType (return IsStrict)) wrappedIvarTypes
        ivars = [ (name, ty) | Outlet name ty <- members ]
             ++ [ (name, ty) | InstanceVariable name ty <- members ]
        wrappedIvarTypes = [ conT "GHC.IOBase:MVar" `appT` ty | (_,ty) <- ivars ]
        ivarNames = [ name | (name,_) <- ivars ]
        clsTy = conT name `appT` conT "GHC.Base:()"
        instTy = conT instanceDataName
        nIVars = length ivarNames
        
        declaredIVars = zipWith declareIVar ivarNames [1..]
        declareIVar ivar n = valD (VarP ('_' : ivar)) (normalB [| IVar $(getNth n) |]) []
            where
                getNth n = lamE [conP instanceDataName args] (varE $ "arg" ++ show n)
                args = [ varP $ "arg" ++ show i | i <- [1..nIVars] ]
        
        initIVars = doE (map initIVar ivarNames ++ [noBindS [| return $(wrap) |]])
            where
                wrap = foldl appE (conE instanceDataName) (map varE ivarNames)
                initIVar ivar = bindS (varP ivar) [| newMVar nil |]

data Method = ImplementedMethod SelectorInfo String
            | GetterMethod String
            | SetterMethod String

mkClassExportAction name members =
        [| 
            do
                super <- getClassByName $(varE $ "super_" ++ name)
                ivars <- makeDefaultIvarList
                imethods <- makeMethodList (nIMethods+3)
                cmethods <- makeMethodList nCMethods
                setHaskellRetainMethod imethods 0
                setHaskellReleaseMethod imethods 1
                setHaskellDataMethod imethods 2 super (
                        Just ($(typedInitIvars) >>= return . toDyn)
                    )
                $(fillMethodList False 3 [|imethods|] instanceMethods) 
                $(fillMethodList True 0 [|cmethods|] classMethods)
                clsname <- newCString name
                newClass super clsname 0 ivars imethods cmethods
        |]
    where
        typedInitIvars = [|initializeInstanceVariables|] `sigE` (conT "GHC.IOBase:IO" `appT` conT (name ++ "_IVARS"))
    
        outlets = [ name | Outlet name _ <- members ]
        classMethods =    [ ImplementedMethod info def | ClassMethod    info def <- members ] 

        explicitInstanceMethods = [ (info, def) | InstanceMethod info def <- members ]         
        instanceMethodNames = map (selectorInfoObjCName . fst) explicitInstanceMethods
        instanceMethods =
                [ ImplementedMethod i d | (i,d) <- explicitInstanceMethods ] 
             ++ [ GetterMethod ivar | ivar <- outlets,
                                      not (ivar `elem` instanceMethodNames) ]
             ++ [ SetterMethod ivar | ivar <- outlets,
                                      not (setterNameFor ivar
                                           `elem` instanceMethodNames) ]
  
        nIMethods = length instanceMethods
        nCMethods = length classMethods
        
            -- GHC fails with Prelude.last if we pass it an empty doE
        fillMethodList isClassMethod firstIdx objCMethodList [] = [| return () |]  
        fillMethodList isClassMethod firstIdx objCMethodList methods =
            doE $
            map (noBindS . exportMethod isClassMethod objCMethodList)
                (zip methods [firstIdx..])
                
        exportMethod isClassMethod objCMethodList (ImplementedMethod selectorInfo methodDefinition,num) =
                exportMethod' isClassMethod objCMethodList num methodBody
                              nArgs isUnit impTypeName selExpr cifExpr
            where
                methodBody = varE methodDefinition
                selName = selectorInfoHaskellName selectorInfo
                nArgs = selectorInfoNArgs selectorInfo
                isUnit = selectorInfoIsUnit selectorInfo
                
                impTypeName = "ImpType_" ++ selName
                selExpr = [| selectorInfoSel $(varE $ "info_" ++ selName) |]
                cifExpr = [| selectorInfoCif $(varE $ "info_" ++ selName) |]
        
        exportMethod isClassMethod objCMethodList (GetterMethod ivarName, num) =
                exportMethod' isClassMethod objCMethodList num
                              (varE (thModulePrefix "ExportClass" "getAsID") `appE` varE ('_':ivarName))
                              0 False (thModulePrefix "ExportClass" "GetVarImpType")
                              [| getSelectorForName ivarName |]
                              (varE (thModulePrefix "ExportClass" "getVarCif"))
            
        exportMethod isClassMethod objCMethodList (SetterMethod ivarName, num) =
                exportMethod' isClassMethod objCMethodList num
                              (varE (thModulePrefix "ExportClass" "setAsID") `appE` varE ('_':ivarName))
                              1 True (thModulePrefix "ExportClass" "SetVarImpType")
                              [| getSelectorForName setterName |]
                              (varE (thModulePrefix "ExportClass" "setVarCif"))
            where
                setterName = setterNameFor ivarName
        
        setterNameFor ivarName = "set" ++ toUpper (head ivarName) : tail ivarName
            
                
        exportMethod' isClassMethod objCMethodList num methodBody
                       nArgs isUnit impTypeName selExpr cifExpr =
            [|
                setMethodInList $(objCMethodList)
                                num
                                $(selExpr)
                                (objCMethodType $(typed [|undefined|]))
                                $(cifExpr)
                                ($(lamE [varP "cif", varP "ret", varP "args"] marshal))
            |]
            where
                marshal = [| exceptionHaskellToObjC $(marshal') |]
                marshal' = doE $ getArg ("self",0)
                                 : map getArg (zip arguments [2..])
                                 ++ invokeAndReturn
                
                
                arguments = [ "arg" ++ show i | i <- [1..nArgs] ]
                
                invokeAndReturn
                    | isUnit =
                        [noBindS typedBodyWithArgs]
                    | otherwise =
                        [
                            bindS (VarP "result") typedBodyWithArgs,
                            noBindS [| setMarshalledRetval
                                            $(varE "ret") $(varE "result") |]
                        ]
                
                typedBodyWithArgs = foldl1 appE (typed methodBody
                                                : map varE (arguments ++ ["self"])) 
                    where
                        arguments = ["arg" ++ show i | i <- [1..nArgs]]
                
                typed thing = thing
                            `sigE` (conT impTypeName
                                    `appT` (targetType
                                            `appT` conT "GHC.Base:()")
                                    `appT` (instanceType
                                            `appT` conT "GHC.Base:()")
                                     )
                
                targetType   | isClassMethod = conT $ name ++ "Class"
                             | otherwise     = instanceType
                             
                instanceType = conT name
                
                getArg (argname, argnum) =
                    bindS (VarP argname)
                          [| getMarshalledArgument $(varE "args") argnum |]
