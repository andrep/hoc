module BindingScript(
        BindingScript(bsHiddenFromPrelude),
        getSelectorOptions,
        SelectorOptions(..),
        readBindingScript
    ) where

import Data.FiniteMap
import Data.Set

import Text.ParserCombinators.Parsec.Language(haskell)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec

data BindingScript = BindingScript {
        bsHiddenFromPrelude :: Set String,
        bsTopLevelOptions :: SelectorOptions
    }
    
data SelectorOptions = SelectorOptions {
        soNameMappings :: FiniteMap String String,
        soCovariantSelectors :: Set String,
        soHiddenSelectors :: Set String
    }
    
getSelectorOptions :: BindingScript -> String -> SelectorOptions

getSelectorOptions bindingScript clsName =
    bsTopLevelOptions bindingScript

tokenParser = haskell

selector tp = lexeme tp $ do
                c <- letter
                s <- many (alphaNum <|> oneOf "_:")
                return (c:s)

idList keyword = do
    symbol tokenParser keyword
    many1 (identifier tokenParser)

data Statement = HidePrelude String
               | Covariant String
               | Hide String
               | Rename String String 

extractSelectorOptions statements =
    SelectorOptions {
            soNameMappings = listToFM [ (objc, haskell)
                                      | Rename objc haskell <- statements ],
            soCovariantSelectors = mkSet $ [ ident 
                                           | Covariant ident <- statements ],
            soHiddenSelectors = mkSet $ [ ident | Hide ident <- statements ]
    }

hidePrelude = fmap (map HidePrelude) $ idList "hidePrelude"
    
rename = do
    symbol tokenParser "rename"
    objc <- selector tokenParser
    haskell <- identifier tokenParser
    return [Rename objc haskell]
    
covariant = fmap (map Covariant) $ idList "covariant"
hide = fmap (map Hide) $ idList "hide"

statement = do
    result <- hidePrelude <|> rename <|> covariant <|> hide
    semi tokenParser
    return result

bindingScript = do
    statements <- fmap concat $ many statement
    eof
    
    return $ BindingScript {
            bsHiddenFromPrelude = mkSet [ ident | HidePrelude ident <- statements ],
            bsTopLevelOptions = extractSelectorOptions statements
        }

readBindingScript fn = do
    either <- parseFromFile bindingScript fn
    case either of
        Left err -> error (show err)
        Right result -> return result
