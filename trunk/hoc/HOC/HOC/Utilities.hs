module HOC.Utilities where

import HOC.Base
import HOC.Arguments

x # f = f x
obj #* msg = obj # msg >>= \newObj -> withExportedArgument newObj releaseObject >> return newObj
