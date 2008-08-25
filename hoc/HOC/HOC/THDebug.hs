{-# OPTIONS -fno-monomorphism-restriction #-}

module HOC.THDebug
    (
    cleanNames,
    ppQ,
    sppQ,
    ppAST,
    sppAST,
    showQ,
    sshowQ,
    showAST,
    sshowAST,
    assertQ,
    ) where

import Language.Haskell.TH
import Data.Generics
import Control.Monad


cleanNames = everywhere (mkT (mkName . nameBase))

sppQ :: (Ppr a, Data a) => Q a->IO String
sppQ q = return .  pprint . cleanNames =<< runQ q
ppQ q = putStrLn =<< sppQ q

sppAST ast = pprint . cleanNames $ ast
ppAST ast = putStrLn $ sppAST ast

sshowQ q = return . show . cleanNames =<< runQ q
sshowAST ast = show . cleanNames $ ast

showQ q = putStrLn =<< sshowQ q
showAST ast = putStrLn $ sshowAST ast

assertQ :: Bool -> String -> Q ()
assertQ b msg = unless b $ fail msg
