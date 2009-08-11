--X catchNS

import HOC.Exception        ( WrappedNSException(..), catchWrappedNSException )
-- CUT HERE

catchNS :: IO a -> (NSException () -> IO a) -> IO a

catchNS action handler
    = action `catchWrappedNSException` \(WrappedNSException exc) -> handler (castObject exc)
