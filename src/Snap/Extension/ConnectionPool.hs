{-# LANGUAGE Rank2Types #-}

{-|

'Snap.Extension.ConnectionPool' exports the 'MonadConnectionPool' interface
which allows you to use HDBC connections in your application. These
connections are pooled and only created once. The interface's only operation
is 'withConnection'.

'Snap.Extension.ConnectionPool.ConnectionPool' contains the only
implementation of this interface and can be used to turn your application's
monad into a 'MonadConnectionPool'.

-}

module Snap.Extension.ConnectionPool (MonadConnectionPool(..)) where

import           Database.HDBC
import           Snap.Types

------------------------------------------------------------------------------
-- | The 'MonadConnectionPool' type class. Minimal complete definition:
-- 'withConnection'.
class MonadSnap m => MonadConnectionPool m where
    -- | Given an action, wait for an available connection from the pool and
    -- execute the action. Return the result.
    withConnection :: (forall c. IConnection c => c -> IO a) -> m a
