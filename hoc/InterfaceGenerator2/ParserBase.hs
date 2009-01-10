module ParserBase where

import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Token
import Control.Monad.Trans( lift )
import Messages
import qualified Text.PrettyPrint.HughesPJ as PP
import SrcPos

type ParseEnvironment = Map.Map String Integer

emptyParseEnvironment :: ParseEnvironment
emptyParseEnvironment = Map.empty

type Parser a = ParsecT String ParseEnvironment Messages a

type HOCTokenParser = GenTokenParser String ParseEnvironment Messages

runParserSimple parser fileName text
    = fst $ runMessages $ runParserT parser emptyParseEnvironment fileName text

lookupIntegerConstant :: String -> Parser Integer
lookupIntegerConstant name = getState >>= mapLookup name
    where
        -- ghc 6.10's containers package no longer allows arbitrary
        -- monads in its return types
        mapLookup k v = case Map.lookup k v of
            Nothing -> fail "Integer constant not found"
            Just x  -> return x

defineIntegerConstant :: String -> Integer -> Parser ()
defineIntegerConstant name value = modifyState (Map.insert name value)

parseWarning :: String -> Parser ()
parseWarning msg
    = do
        pos <- getPosition
        lift (message $ pprSourcePos (parsecPosToSrcPos pos) 
                     PP.<> PP.text (": " ++ msg))

