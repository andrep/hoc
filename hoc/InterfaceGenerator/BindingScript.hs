module BindingScript(
        BindingScript(bsHiddenFromPrelude),
        getSelectorOptions,
        SelectorOptions(..),
        readBindingScript
    ) where

import Data.FiniteMap
import Data.Set
import Data.Char(isSpace)

data BindingScript = BindingScript {
        bsNameMappings :: FiniteMap String String,
        bsHiddenFromPrelude :: Set String,
        bsCovariantSelectors :: Set String,
        bsHiddenSelectors :: Set String
    }
    
data SelectorOptions = SelectorOptions {
        soNameMappings :: FiniteMap String String,
        soCovariantSelectors :: Set String,
        soHiddenSelectors :: Set String
    }
    
getSelectorOptions :: BindingScript -> String -> SelectorOptions

getSelectorOptions bindingScript clsName =
    SelectorOptions {
        soNameMappings = bsNameMappings bindingScript,
        soCovariantSelectors = bsCovariantSelectors bindingScript,
        soHiddenSelectors = bsHiddenSelectors bindingScript
    }
    
-- TODO: replace this by a proper parser, or at least report errors
    
readBindingScript fn = do
    scriptLines <- fmap (map words .
                        filter ((/= '#') . head) .
                        filter (any (not . isSpace)) .
                        lines) $
                  readFile fn
    return $ BindingScript {
            bsNameMappings = listToFM [ (objc, haskell)
                                      | ["rename", objc, haskell] <- scriptLines ],
            bsHiddenFromPrelude = mkSet $ concat 
                                  [ xs | ("hidePrelude" : xs) <- scriptLines ],
            bsCovariantSelectors = mkSet $
                                   concat [ xs 
                                          | ("covariant" : xs) <- scriptLines ],
            bsHiddenSelectors = mkSet $ concat [ xs | ("hide" : xs) <- scriptLines ]
        }

