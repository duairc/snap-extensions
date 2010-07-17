{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

{-|

'Snap.Extension.DBPool' provides your application with database connections
managed by a pool. It extends your monad with the 'withConnection' operation.

This extension does not depend on any other extensions.

-}

module Snap.Extension.DBPool
  ( MonadDBPool(..)
  , HasDBPoolState(..)
  , DBPoolState
  , dbPoolRunner
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
class MonadSnap m => MonadDBPool m where
    -- | Given an action, wait for an available connection from the pool and
    -- execute the action. Return the result.
    withConnection :: (forall c. IConnection c => c -> IO a) -> m a


------------------------------------------------------------------------------
instance HasDBPoolState s => MonadDBPool (SnapExtend s) where
    withConnection f = do
        (DBPoolState mkConn chan _) <- asks getDBPoolState
        conn@(Connection c) <- liftIO $ readChan chan >>= maybe mkConn return
        liftIO $ f c `finally` (commit c >> writeChan chan (Just conn))


------------------------------------------------------------------------------
class HasDBPoolState s where
    getDBPoolState :: s -> DBPoolState
    setDBPoolState :: DBPoolState -> s -> s


------------------------------------------------------------------------------
-- | A pool of database connections. This stores the 'IO' action for opening
-- new database connections, and a pool of (possibly unopened) connections.
data DBPoolState = DBPoolState (IO Connection) (Chan (Maybe Connection)) Int


------------------------------------------------------------------------------
instance RunnerState DBPoolState where
    extensionId                         = const "Snap.Extension.DBPool"

    mkCleanup (DBPoolState _ chan size) = replicateM_ size $ do
        readChan chan >>= maybe (return ()) (\(Connection c) -> disconnect c)

    mkReload (DBPoolState _ chan size)  = replicateM_ size $ do
        readChan chan >>= maybe (return ()) (\(Connection c) -> disconnect c)
        writeChan chan Nothing


------------------------------------------------------------------------------
-- | Create a lazy connection pool with the specified maximum number of
-- connections. This will not create actual connections until requested, but
-- it will go round-robin through the connection pool to create them. 
-- This should suffice for both production (one pool for all requests until
-- server shutdown) and development (one pool per request) cases.
dbPoolRunner :: IConnection a => IO a -> Int -> Runner DBPoolState
dbPoolRunner mkConn size = do
    chan <- liftIO newChan
    liftIO $ replicateM_ size $ writeChan chan Nothing
    mkRunner $ DBPoolState (mkConn >>= return . Connection) chan size
