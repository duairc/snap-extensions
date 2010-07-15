{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Timer' is a trivial Snap extension which adds the operation
'startTime' to your application's monad which simply returns the time at which
the application was last loaded.

-}

module Snap.Extension.Timer
  ( MonadTimer(..)
  , HasTimerState(..)
  , TimerState
  , timerRunner
  , startTimeSplice
  , currentTimeSplice
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import qualified Data.ByteString.UTF8 as U
import           Data.Time.Clock
import           Snap.Extension
import           Snap.Types
import           Text.Templating.Heist
import           Text.XML.Expat.Tree hiding (Node)


------------------------------------------------------------------------------
-- | The 'MonadTimer' typeclass. It contains the 'startTime' function.
class MonadSnap m => MonadTimer m where
    -- | The time at which your application was last loaded.
    startTime :: m UTCTime


------------------------------------------------------------------------------
instance HasTimerState s => MonadTimer (SnapExtend s) where
    startTime = do
        (TimerState t) <- asks getTimerState
        return t


------------------------------------------------------------------------------
class HasTimerState s where
    getTimerState :: s -> TimerState
    setTimerState :: TimerState -> s -> s


------------------------------------------------------------------------------
-- | A simple wrapper around a 'UTCTime' containing the start time.
newtype TimerState = TimerState UTCTime


------------------------------------------------------------------------------
instance RunnerState TimerState where
    extensionId = const "Snap.Extension.Timer"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()


------------------------------------------------------------------------------
-- | The runner for the Timer extension.
timerRunner :: Runner TimerState
timerRunner = do
    time <- liftIO getCurrentTime
    mkRunner $ TimerState time


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the start time.
startTimeSplice :: MonadTimer m => Splice m
startTimeSplice = do
    time <- lift startTime
    return $ [mkText $ U.fromString $ show $ time]


------------------------------------------------------------------------------
-- | For your convenience, a splice which shows the current time.
currentTimeSplice :: MonadTimer m => Splice m
currentTimeSplice = do
    time <- lift $ liftIO getCurrentTime
    return $ [mkText $ U.fromString $ show $ time]
