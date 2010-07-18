{-|

'Snap.Extension.Less' exports the 'MonadLess' interface which allows to
integrate Less stylesheets (see http:\/\/lesscss.org\/) into your Snap
application. The interface's operations are 'lessServe' and 'lessServeSingle'.

'Snap.Extension.Less.Less' contains the only implementation of this interface
and can be used to turn your application's monad into a 'MonadLess'.

-}

module Snap.Extension.Less (MonadLess(..)) where

import           Data.ByteString (ByteString)
import           Snap.Types

------------------------------------------------------------------------------
-- | The 'MonadLess' type class. Minimal complete definition: 'lessServe', 
-- 'lessServeSingle'.
class MonadSnap m => MonadLess m where
    -- | Analogous to 'fileServe'. If the stylesheet specified in the request
    -- path is not found, it returns 'empty'.
    lessServe       :: m ()

    -- | Analogous to 'fileServeSingle'. If the given stylesheet is not found,
    -- this throws an error.
    lessServeSingle :: ByteString -> m ()
