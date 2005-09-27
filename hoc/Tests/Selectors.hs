module Selectors where

import HOC

$(declareSelector "throwHaskellException" [t| IO () |])
$(declareSelector "throwNSException" [t| IO () |])
