{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.ConnectionPool.ConnectionPool' is an implementation of the
'MonadConnectionPool' interface defined in 'Snap.Extension.ConnectionPool'.

As always, to use, add 'ConnectionPoolState' to your application's state,
along with an instance of 'HasConnectionPoolState' for your application's
state, making sure to use a 'connectionPoolRunner' in your application's
'Runner', and then you're ready to go.

The 'ConnectionPoolState' has a maximum size associated with it, but it won't
not get filled up until necessary. It will not create actual connections until
requested, it will go round-robin through the connection pool to create them. 
This should suffice for both production (one pool for all requests until
server shutdown) and development (one pool per request) cases.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Snap.Extension.ConnectionPool.ConnectionPool
  ( HasConnectionPoolState(..)
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
import           Snap.Extension.ConnectionPool
import           Snap.Types


------------------------------------------------------------------------------
-- | An existential type, inside which can be put any instance of
-- 'IConnection'. This greatly simplifies the interface to
-- 'HasConnectionPoolState', but means that 'withConnection', defined in
-- 'Snap.Extension.ConnectionPool', must have a Rank 2 type
-- @forall c. IConnection c => c -> IO a@.
data Connection = forall c. IConnection c => Connection c


------------------------------------------------------------------------------
-- | Your application's state must include a 'ConnectionPoolState' in order
-- for your application to be a 'MonadConnectionPoolState'.
data ConnectionPoolState = ConnectionPoolState
    { _mkConn :: IO Connection
    , _chan   :: Chan (Maybe Connection)
    , _size   :: Int
    }


------------------------------------------------------------------------------
-- | For you appliaction's monad to be a 'MonadConnectionPool', your
-- application's state needs to be an instance of 'HasConnectionPoolState'.
-- Minimal complete definition: 'getConnectionPoolState',
-- 'setConnectionPoolState'.
class HasConnectionPoolState s where
    getConnectionPoolState :: s -> ConnectionPoolState
    setConnectionPoolState :: ConnectionPoolState -> s -> s
   


------------------------------------------------------------------------------
-- | The 'Runner' for 'ConnectionPoolState'. It takes two arguments, an 'IO'
-- action which creates an instance of 'IConnection', and the desired maximum
-- size of the pool.
connectionPoolRunner :: IConnection a
                     => IO a -> Int -> Runner ConnectionPoolState
connectionPoolRunner mkConn size = do
    chan <- liftIO newChan
    liftIO $ replicateM_ size $ writeChan chan Nothing
    mkRunner $ ConnectionPoolState (mkConn >>= return . Connection) chan size


------------------------------------------------------------------------------
instance RunnerState ConnectionPoolState where
    extensionId = const "ConnectionPool/ConnectionPool"

    mkCleanup (ConnectionPoolState _ chan size) = replicateM_ size $ do
        readChan chan >>= maybe (return ()) (\(Connection c) -> disconnect c)

    mkReload (ConnectionPoolState _ chan size)  = replicateM_ size $ do
        readChan chan >>= maybe (return ()) (\(Connection c) -> disconnect c)
        writeChan chan Nothing


------------------------------------------------------------------------------
instance HasConnectionPoolState s => MonadConnectionPool (SnapExtend s) where
    withConnection f = do
        (ConnectionPoolState mkConn chan _) <- asks getConnectionPoolState
        conn@(Connection c) <- liftIO $ readChan chan >>= maybe mkConn return
        liftIO $ f c `finally` (commit c >> writeChan chan (Just conn))


------------------------------------------------------------------------------
instance (MonadSnap m, HasConnectionPoolState s) => MonadConnectionPool (ReaderT s m) where
    withConnection f = do
        (ConnectionPoolState mkConn chan _) <- asks getConnectionPoolState
        conn@(Connection c) <- liftIO $ readChan chan >>= maybe mkConn return
        liftIO $ f c `finally` (commit c >> writeChan chan (Just conn))
