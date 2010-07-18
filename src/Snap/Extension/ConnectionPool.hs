{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

{-|

'Snap.Extension.ConnectionPool' provides your application with database connections
managed by a pool. It extends your monad with the 'withConnection' operation.

This extension does not depend on any other extensions.

-}

module Snap.Extension.ConnectionPool
  ( MonadConnectionPool(..)
  , HasConnectionPoolState(..)
  , ConnectionPoolState
  , connectionPoolRunner
  ) where

import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Exception
import           Database.HDBC
import           Snap.Extension
import           Snap.Types


------------------------------------------------------------------------------
data Connection = forall c. IConnection c => Connection c


------------------------------------------------------------------------------
class MonadSnap m => MonadConnectionPool m where
    -- | Given an action, wait for an available connection from the pool and
    -- execute the action. Return the result.
    withConnection :: (forall c. IConnection c => c -> IO a) -> m a


------------------------------------------------------------------------------
instance HasConnectionPoolState s => MonadConnectionPool (SnapExtend s) where
    withConnection f = do
        (ConnectionPoolState mkConn chan _) <- asks getConnectionPoolState
        conn@(Connection c) <- liftIO $ readChan chan >>= maybe mkConn return
        liftIO $ f c `finally` (commit c >> writeChan chan (Just conn))


------------------------------------------------------------------------------
class HasConnectionPoolState s where
    getConnectionPoolState :: s -> ConnectionPoolState
    setConnectionPoolState :: ConnectionPoolState -> s -> s


------------------------------------------------------------------------------
-- | A pool of database connections. This stores the 'IO' action for opening
-- new database connections, and a pool of (possibly unopened) connections.
data ConnectionPoolState
    = ConnectionPoolState (IO Connection) (Chan (Maybe Connection)) Int


------------------------------------------------------------------------------
instance RunnerState ConnectionPoolState where
    extensionId = const "Snap.Extension.ConnectionPool"

    mkCleanup (ConnectionPoolState _ chan size) = replicateM_ size $ do
        readChan chan >>= maybe (return ()) (\(Connection c) -> disconnect c)

    mkReload (ConnectionPoolState _ chan size)  = replicateM_ size $ do
        readChan chan >>= maybe (return ()) (\(Connection c) -> disconnect c)
        writeChan chan Nothing


------------------------------------------------------------------------------
-- | Create a lazy connection pool with the specified maximum number of
-- connections. This will not create actual connections until requested, but
-- it will go round-robin through the connection pool to create them. 
-- This should suffice for both production (one pool for all requests until
-- server shutdown) and development (one pool per request) cases.
connectionPoolRunner :: IConnection a
                     => IO a -> Int -> Runner ConnectionPoolState
connectionPoolRunner mkConn size = do
    chan <- liftIO newChan
    liftIO $ replicateM_ size $ writeChan chan Nothing
    mkRunner $ ConnectionPoolState (mkConn >>= return . Connection) chan size
