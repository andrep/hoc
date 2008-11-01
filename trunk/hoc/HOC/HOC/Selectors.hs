{-# LANGUAGE TemplateHaskell #-}
module HOC.Selectors where

import HOC.TH
import HOC.SelectorMarshaller ( SelectorInfo(..) )

sel n = [| selectorInfoSel $(varE selInfo) |]
    where selInfo = infoName `fromSameModuleAs_v` n
          infoName = "info_" ++ nameBase n
