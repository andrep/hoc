module BindingScript(
        BindingScript(bsHiddenFromPrelude, bsHiddenEnums, bsAdditionalTypes),
        getSelectorOptions,
        SelectorOptions(..),
        readBindingScript
    ) where

import SyntaxTree(
        SelectorListItem(InstanceMethod, ClassMethod),
        Selector(..)
    )
import qualified Parser(selector)

import Control.Monad(when)
import qualified Data.Map as Map
import Data.Set(Set, union, mkSet, setToList)
import Data.List(intersperse)

import Text.ParserCombinators.Parsec.Language(haskellStyle)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec

data BindingScript = BindingScript {
        bsHiddenFromPrelude :: Set String,
        bsHiddenEnums :: Set String,
        bsTopLevelOptions :: SelectorOptions,
        bsAdditionalTypes :: [(String, String)],
        bsClassSpecificOptions :: Map.Map String SelectorOptions
    }
    
data SelectorOptions = SelectorOptions {
        soNameMappings :: Map.Map String String,
        soCovariantSelectors :: Set String,
        soHiddenSelectors :: Set String,
        soChangedSelectors :: Map.Map String Selector
    }
    
getSelectorOptions :: BindingScript -> String -> SelectorOptions

getSelectorOptions bindingScript clsName =
        case Map.lookup clsName (bsClassSpecificOptions bindingScript) of
            Just opt -> SelectorOptions {
                            soNameMappings = soNameMappings opt
                                    `Map.union` soNameMappings top,
                            soCovariantSelectors = soCovariantSelectors opt
                                           `union` soCovariantSelectors top,
                            soHiddenSelectors = soHiddenSelectors opt
                                        `union` soHiddenSelectors top,
                            soChangedSelectors = soChangedSelectors opt
                                        `Map.union` soChangedSelectors top
                        }
            Nothing -> top
    where
        top = bsTopLevelOptions bindingScript

tokenParser = makeTokenParser $ haskellStyle { identStart = letter <|> char '_' }

selector tp = lexeme tp $ do
                c <- letter <|> char '_'
                s <- many (alphaNum <|> oneOf "_:")
                return (c:s)
qualified tp = do x <- identifier tp
                  xs <- many (symbol tp "." >> identifier tp)
                  return (concat $ intersperse "." $ x : xs)
                  
idList keyword = do
    try $ symbol tokenParser keyword
    many1 (identifier tokenParser)

data Statement = HidePrelude String
               | Covariant String
               | Hide String
               | Rename String String
               | ClassSpecific String SelectorOptions
               | ReplaceSelector Selector
               | Type String String
               | HideEnum String

extractSelectorOptions statements =
    SelectorOptions {
            soNameMappings = Map.fromList [ (objc, haskell)
                                      | Rename objc haskell <- statements ],
            soCovariantSelectors = mkSet $ [ ident 
                                           | Covariant ident <- statements ],
            soHiddenSelectors = mkSet $ [ ident | Hide ident <- statements ],
            soChangedSelectors = Map.fromList [ (selName sel, sel)
                                          | ReplaceSelector sel <- statements ]
    }

hidePrelude = fmap (map HidePrelude) $ idList "hidePrelude"
    
rename = do
    symbol tokenParser "rename"
    objc <- selector tokenParser
    haskell <- identifier tokenParser
    return [Rename objc haskell]
    
covariant = fmap (map Covariant) $ idList "covariant"
hide = do
    try $ symbol tokenParser "hide"
    fmap (map Hide) $ many1 (selector tokenParser)

hideEnums = fmap (map HideEnum) $ idList "hideEnums"
    
replaceSelector = do
    thing <- try Parser.selector
    let sel = case thing of
                InstanceMethod sel -> sel
                ClassMethod sel -> sel
    return [ReplaceSelector sel]

typ = do
    try $ symbol tokenParser "type"
    typ <- identifier tokenParser
    mod <- qualified tokenParser
    return [Type typ mod]

statement = classSpecificOptions <|> replaceSelector <|> do
    result <- hidePrelude  <|> hideEnums <|> rename <|> covariant <|> hide <|> typ
    semi tokenParser
    return result

classSpecificOptions = do
    try $ symbol tokenParser "class"
    clsname <- identifier tokenParser
    statements <- braces tokenParser (fmap concat $ many statement)
    
    let wrongThings = [ () | HidePrelude _ <- statements ]
                   ++ [ () | ClassSpecific _ _ <- statements ]
                   ++ [ () | HideEnum _ <- statements ]
    
    when (not $ null wrongThings) $ fail "illegal thing in class block"
    
    return [ClassSpecific clsname (extractSelectorOptions statements)]

bindingScript = do
    statements <- fmap concat $ many statement
    eof

    let wrongThings = [ () | ReplaceSelector _ <- statements ]
    
    return $ BindingScript {
            bsHiddenFromPrelude = mkSet [ ident | HidePrelude ident <- statements ],
            bsHiddenEnums = mkSet [ ident | HideEnum ident <- statements ],
            bsTopLevelOptions = extractSelectorOptions statements,
            bsAdditionalTypes = [ (typ, mod) | Type typ mod <- statements ],
            bsClassSpecificOptions = Map.fromList [ (cls, opt)
                                              | ClassSpecific cls opt <- statements ]
        }

readBindingScript fn = do
    either <- parseFromFile bindingScript fn
    case either of
        Left err -> error (show err)
        Right result -> print (setToList $ bsHiddenEnums result) >> return result
