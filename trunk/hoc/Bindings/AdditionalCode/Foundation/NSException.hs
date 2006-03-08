--X catchNS

import Control.Exception    ( catchDyn )
import HOC.Exception        ( WrappedNSException(..) )
-- CUT HERE

catchNS :: IO a -> (NSException () -> IO a) -> IO a

catchNS action handler
    = action `catchDyn` \(WrappedNSException exc) -> handler (castObject exc)
