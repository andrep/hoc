module TestHOCDeclareSelectorForward
    where

import HOC.CannedCIFs
import HOC.THDebug
import Language.Haskell.TH

data NewType a  = NewType a

type TypeAlias a = NewType a 

type TypeAlias' a = TypeAlias a

-- this is a compiler program!
-- first we create a bunch of type declarations
-- We write some code to print out their correct transformed version
-- (showAST and ppAST are useful here)
-- We then use the output from the compiler to fill in our answers, then code 
-- up a fail statement if they do not match
firstOrderAlias = [t| TypeAlias Int -> IO Int |]
firstOrderAliasAnswer = AppT (AppT ArrowT (AppT (ConT ''NewType) (ConT ''Int))) (AppT (ConT ''IO) (ConT ''Int))

simpleForall = return (ForallT
            [mkName "a"]
            []
            (
                AppT (AppT ArrowT
                (AppT (ConT ''TypeAlias) (VarT (mkName "a"))))
                (AppT (ConT ''IO) (VarT (mkName "a")))
            ))
simpleForallAnswer = AppT (AppT ArrowT (AppT (ConT ''NewType) (VarT $ mkName "a"))) (AppT (ConT ''IO) (VarT $ mkName "a"))

secondOrderAlias = [t| TypeAlias' Int -> IO Int |]
secondOrderAliasAnswer = AppT (AppT ArrowT (AppT (ConT ''NewType) (ConT ''Int))) (AppT (ConT ''IO) (ConT ''Int))

complexType = do 
    t1 <- firstOrderAlias
    t2 <- simpleForall
    return (AppT (AppT ArrowT t1) t2)
complexTypeAnswer = AppT (AppT ArrowT (AppT (AppT ArrowT (AppT (ConT ''NewType) (ConT ''Int))) (AppT (ConT ''IO) (ConT ''Int)))) (AppT (AppT ArrowT (AppT (ConT ''NewType) (VarT $ mkName "a"))) (AppT (ConT ''IO) (VarT $ mkName "a" )))

dummy = do
    runIO $ putStrLn "Begin TH tests!"
    compareTypes firstOrderAliasAnswer =<< firstOrderAlias
    compareTypes simpleForallAnswer =<< simpleForall
    compareTypes secondOrderAliasAnswer =<< secondOrderAlias
    compareTypes complexTypeAnswer =<< complexType
--    t' <- expandSynonyms =<< [t| TypeAlias' Int -> IO Int |]
--    runIO $ ppAST t'
    runIO $ putStrLn "End TH Tests!"
    [|()|]

compareTypes ans t = do
    runIO $ do
        putStr "Begin test expandSynonyms\nfrom: "
        putStrLn $ sppAST t
        putStr "to: "
        putStrLn $ sppAST ans
    t' <- expandSynonyms t
    if (t' == ans)
        then runIO $ putStrLn "Success!"
        else fail $ "Failed! found instead: " ++ sppAST t'

    
