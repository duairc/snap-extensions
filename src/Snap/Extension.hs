{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Extension
  ( SnapExtend
  , Runner
  , RunnerState(..)
  , runRunner
  , runRunnerHint
  , mkRunner
  , defaultReloadHandler
  , nullReloadHandler
  ) where

import           Control.Applicative
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U
import           Data.Monoid
import           Prelude hiding (catch)
import           Snap.Iteratee ((>.), enumBS)
import           Snap.Types
import           System.IO


------------------------------------------------------------------------------
-- | A 'SnapExtend' is a 'MonadReader' and a 'MonadSnap' whose environment is
-- the application state for a given progam. You would usually type alias
-- @SnapExtend AppState@ to something like @App@ to form the monad in which
-- you write your application.
newtype SnapExtend s a = SnapExtend (ReaderT s Snap a)
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadIO
    , MonadCatchIO
    , MonadSnap
    , MonadReader s
    )


------------------------------------------------------------------------------
-- | The 'Runner' monad. This can be used for constructing values which also
-- have cleanup\/destroy and reload functions.
newtype Runner s = Runner
    (Bool -> IO (Either s (s, IO (), IO [(ByteString, Maybe ByteString)])))


------------------------------------------------------------------------------
-- | Values of types which are instances of 'RunnerState' have
-- cleanup\/destroy and reload actions associated with them.
class RunnerState s where
    extensionId :: s -> ByteString
    mkCleanup   :: s -> IO ()
    mkReload    :: s -> IO ()


------------------------------------------------------------------------------
-- | Although it has the same type signature, this is not the same as 'return'
-- in the 'Runner' monad. Return simply lifts a value into the 'Runner' monad,
-- but this lifts the value and its destroy\/reload actions. Use this when
-- making your own 'Runner' actions.
mkRunner :: RunnerState s => s -> Runner s
mkRunner s = Runner $ \v -> setup v $ Right (s, cleanup v, reload v)
  where
    handler          :: SomeException -> IO (Maybe ByteString)
    handler e        = return $ Just $ U.fromString $ show e
    maybeCatch m     = (m >> return Nothing) `catch` handler
    maybeToMsg       = maybe " done." $ const " failed."
    name             = U.toString $ extensionId s
    cleanup v        = do
        when v $ hPutStr stderr $ "Cleaning up " ++ name ++ "..."
        m <- maybeCatch $ mkCleanup s
        when v $ hPutStrLn stderr $ maybeToMsg m
    reload v         = do
        when v $ hPutStr stderr $ "Reloading " ++ name ++ "..."
        m <- maybeCatch $ mkReload s
        when v $ hPutStrLn stderr $ maybeToMsg m
        return [(extensionId s, m)]
    setup v r        = do
        when v $ hPutStrLn stderr $ "Initializing " ++ name ++ "... done."
        return r


------------------------------------------------------------------------------
-- | Given the runner for your application's state, and a value in the monad
-- formed by 'SnapExtend' wrapped it, this returns a 'Snap' action, a cleanup
-- action and a reload action.
runRunner :: Bool
          -- ^ Verbosity; info is printed to 'stderr' when this is 'True'
          -> Runner s
          -- ^ The Runner value
          -> SnapExtend s ()
          -- ^ An action in your application's monad
          -> IO (Snap (), IO (), IO [(ByteString, Maybe ByteString)])
          -- ^ This is documented thoroughly in the README
runRunner v (Runner r) (SnapExtend m) = r v >>= \e -> case e of
    Left s          -> return (runReaderT m s, return (), return [])
    Right (s, a, b) -> return (runReaderT m s, a, b)


------------------------------------------------------------------------------
-- | Serves the same purpose as 'runRunner', but can be used with Hint. This
-- is explained in the README.
runRunnerHint :: Bool
              -- ^ Verbosity; info is printed to 'stderr' when this is 'True'
              -> Runner s
              -- ^ The Runner value
              -> SnapExtend s ()
              -- ^ An action in your application's monad.
              -> (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
              -- ^ See README and 'defaultReloadHandler'
              -> IO (IO s, s -> IO (), s -> Snap ())
              -- ^ A tuple of values which can be passed to @loadSnapTH@.
runRunnerHint v (Runner r) se@(SnapExtend m) f = r v >>= \e -> case e of
    Left s          -> return (return s, const $ return (), runReaderT m)
    Right (s, a, b) -> let (SnapExtend m') = f b <|> se
                       in return (return s, const a, runReaderT m')


------------------------------------------------------------------------------
instance Functor Runner where
    fmap f (Runner r) = Runner $ \v -> r v >>= \e -> return $ case e of
        Left s          -> Left $ f s
        Right (s, a, b) -> Right (f s, a, b)


------------------------------------------------------------------------------
instance Applicative Runner where
    pure  = return
    (<*>) = ap


------------------------------------------------------------------------------
instance Monad Runner where
    return   = Runner . const . return . Left
    a >>= f  = join' $ fmap f a


------------------------------------------------------------------------------
instance MonadIO Runner where
    liftIO = Runner . const . fmap Left


------------------------------------------------------------------------------
-- | Join for the 'Runner' monad. This is used in the definition of bind for
-- the 'Runner' monad.
join' :: Runner (Runner s) -> Runner s
join' (Runner r) = Runner $ \v -> r v >>= \e -> case e of
    Left  (Runner r')       -> r' v
    Right (Runner r', a, b) -> r' v >>= \e' -> return $ Right $ case e' of
        Left  s           -> (s, a, b)
        Right (s, a', b') -> (s, a' >> a, liftM2 (++) b b')


------------------------------------------------------------------------------
-- | This takes the last value of the tuple returned by 'runRunner', which is
-- a list representing the results of an attempt to reload the application's
-- Snap Extensions, and turns it into a Snap action which displays the these
-- results.
defaultReloadHandler :: MonadSnap m
                     => IO [(ByteString, Maybe ByteString)]
                     -> m ()
defaultReloadHandler ioms = do
    ms <- liftIO $ ioms
    let showE e       = mappend "Error: "  $ U.fromString $ show e
        format (n, m) = mconcat [n, ": ", maybe "Sucess" showE m, "\n"]
        msg           = mconcat $ map format ms
    finishWith $ setContentType "text/plain; charset=utf-8"
        $ setContentLength (fromIntegral $ B.length msg)
        $ modifyResponseBody (>. enumBS msg) emptyResponse


------------------------------------------------------------------------------
-- | Use this reload handler to disable the ability to have a web handler
-- which reloads Snap extensions.
nullReloadHandler :: MonadSnap m
                  => IO [(ByteString, Maybe ByteString)]
                  -> m ()
nullReloadHandler = const pass
