module ParserBase where

import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Token
import Control.Monad.Trans( lift )
import Messages
import qualified Text.PrettyPrint.HughesPJ as PP


type ParseEnvironment = Map.Map String Integer

emptyParseEnvironment :: ParseEnvironment
emptyParseEnvironment = Map.empty

type Parser a = ParsecT String ParseEnvironment Messages a

type HOCTokenParser = GenTokenParser String ParseEnvironment Messages

runParserSimple parser fileName text
    = fst $ runMessages $ runParserT parser emptyParseEnvironment fileName text

lookupIntegerConstant :: String -> Parser Integer
lookupIntegerConstant name = getState >>= Map.lookup name

defineIntegerConstant :: String -> Integer -> Parser ()
defineIntegerConstant name value = modifyState (Map.insert name value)

parseWarning :: String -> Parser ()
parseWarning msg
    = do
        pos <- getPosition
        lift (message $ PP.text (show pos ++ ": " ++ msg))

