{-# LANGUAGE TemplateHaskell #-}
module THTraversal where

import Language.Haskell.TH

import Control.Monad.State
import qualified Data.Map as Map


import ExpandSynonyms

makeTraverser :: TypeQ -> ExpQ -> ExpQ -> ExpQ -> [(TypeQ, ExpQ)] -> ExpQ

makeTraverser theTypeQ wrapCons wrapConsArg primitive specials
    = do
        specials' <- mapM (\(tq, e) -> expandSynonyms tq >>= \t -> return (t,e)) specials
            
        let log = const (return ())
        -- let log = lift . runIO . putStrLn

        let handlerNameForType environment theType0
                = do
                    let theType1 = expandVars environment theType0
                    theType <- lift $ expandSynonyms (return theType1)
                    log $ "Handling type: " ++ show (ppr theType)
                    (functionNames, _) <- get
                    case lookup theType functionNames of
                        Just name -> return name
                        Nothing -> do
                            log $ "Generating for type: " ++ show (ppr theType)
                            name <- lift $ newName "traverse"
                            modify (\ (functionNames, functions) -> ( (theType, name) : functionNames,
                                                                      functions ) )
                                                                      
                            argName <- lift $ newName "thing"
                            let arg = varE argName
                            code <- case lookup theType specials' of
                                Just thing -> return (thing `appE` arg)
                                Nothing -> do
                                    case splitTy theType of
                                        (ConT name, argTypes) -> do
                                            info <- lift $ reify name
                                            case info of
                                                TyConI (DataD cx n argNames cons derivs)
                                                    | not (null cons)
                                                    -> do
                                                        --let conNames = map conName cons
                                                        
                                                        
                                                        let environment' = Map.fromList (zip (map nameBaseWorkaround argNames) argTypes)
                                                                            `Map.union` environment
                                                        log $ "dataD " ++ show n ++ " " ++ show environment'
                                                        matches <- mapM (makeCaseForCon environment') cons
                                                        return [| $(caseE arg matches) |]
                                                _ -> do
                                                    log $ "primitive ConT " ++ show name
                                                    return (primitive `appE` arg)
                                        (ListT, [argType]) -> do
                                            argHandlerName <- handlerNameForType environment argType
                                            return [|
                                                    case $(arg) of
                                                        (x:xs) -> 
                                                            $(wrapConsArg) ($(varE name) xs) $
                                                            $(wrapConsArg) ($(varE argHandlerName) x) $
                                                            $(wrapCons) (:)
                                                        [] -> $(wrapCons) $(arg)
                                                |]
                                        (TupleT n, argTypes) -> do
                                            match1 <- makeCaseForCon2 environment tupP
                                                        (conE $ tupleConstructor $ length argTypes)
                                                        argTypes
                                            return [| $(caseE arg [match1]) |]
                                            
                                        _ -> do
                                            log $ "primitive: " ++ show theType
                                            return (primitive `appE` arg)
                            let fun = funD name [clause [varP argName] (normalB code) []]
                            modify (\ (functionNames, functions) -> ( functionNames,
                                                                      fun : functions ) )
                            return name
                            
            {-
            conName (NormalC n _) = n
            conName (RecC n _) = n
            conName (InfixC _ n _) = n
            conName (ForallC _ _ con) = conName con
            
            forallVars (ForallT vars _ _) = vars
            forallVars _ = []
            
            conForallVars n
                = do
                    DataConI _ ty _ _ <- lift $ reify n
                    return $ forallVars ty-}
                            
            makeCaseForCon environment (NormalC n args) = makeCaseForCon1 environment n (map snd args)
            makeCaseForCon environment (RecC n args) = makeCaseForCon1 environment n (map (\(x,y,z) -> z) args)
            makeCaseForCon environment (InfixC (_,t1) n (_,t2)) = makeCaseForCon1 environment n [t1,t2]
            makeCaseForCon environment (ForallC _ _ con) = makeCaseForCon environment con
            
            makeCaseForCon1 environment name argTypes
                = do
                    log $ "Constructor " ++ show name ++ " " ++ show argTypes
                    makeCaseForCon2 environment (conP name) (conE name) argTypes

            makeCaseForCon2 environment pat con argTypes
                = do
                    argsUnboxed <- lift $ mapM unboxedType argTypes
                    argNames <- lift $ mapM (const $ newName "arg") argTypes
                    let thePat = pat $ map varP argNames
                    argHandlers <- mapM (handlerNameForType environment) argTypes
                    let handledArgs = [ [| $(wrapConsArg) ($(varE h) $(varE a)) |]
                                      | (a,h) <- zip argNames argHandlers ]
                                      
                        body = if or argsUnboxed
                                then primitive `appE` foldl appE con (map varE argNames)
                                else foldl (flip appE) (wrapCons `appE` con) handledArgs
                    when (or argsUnboxed) $
                        lift con >>= \con' -> log $ "primitive because unboxed argument: " ++ show con'
                    return $ match thePat (normalB body) []

            unboxedType ty = case splitTy ty of
                            (ConT name, _) -> do
                                info <- reify name
                                case info of
                                    PrimTyConI _ _ unboxed -> return unboxed
                                    _ -> return False
                            _ -> return False

            tupleConstructor 2 = '(,)
            tupleConstructor 3 = '(,,)
            tupleConstructor 4 = '(,,,)
            tupleConstructor 5 = '(,,,,)
            tupleConstructor 6 = '(,,,,,)

        theType <- theTypeQ               
        (mainName, (names,definitions) ) <- runStateT (handlerNameForType Map.empty theType) ([],[])
        
        letE definitions (varE mainName)
    where
        splitTy ty = loop ty []
            where
                loop (AppT ty arg) args = loop ty (arg : args)
                loop ty0 args = (ty0, args)

        expandVars environment (ForallT names cxt ty)
            = ForallT names (map (expandVars environment') cxt) $ expandVars environment' ty
            where
                environment' = foldr Map.delete environment $ map nameBaseWorkaround names
        expandVars environment (VarT name)
            = case Map.lookup (nameBaseWorkaround name) environment of
                Just ty -> ty
                Nothing -> VarT name
        expandVars environment (AppT a b)
            = AppT (expandVars environment a) (expandVars environment b)
        expandVars environment other
            = other


        nameBaseWorkaround = nameBase

makeEverywhere :: TypeQ -> [(TypeQ, ExpQ)] -> ExpQ
makeEverywhere theType specials = makeTraverser theType [|id|] [|flip ($)|] [|id|] specials

makeForeachM :: TypeQ -> [(TypeQ, ExpQ)] -> ExpQ
makeForeachM theType specials
    = makeTraverser theType
                    [| const (return ()) |]
                    [| flip (>>) |]
                    [| const (return ()) |]
                    specials
