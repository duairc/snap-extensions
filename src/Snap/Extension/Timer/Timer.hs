{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Timer.Timer' is an implementation of the 'MonadTimer'
interface defined in 'Snap.Extension.Timer'.

As always, to use, add 'TimerState' to your application's state, along with an
instance of 'HasTimerState' for your application's state, making sure to use a
'timerRunner' in your application's 'Runner', and then you're ready to go.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Snap.Extension.Timer.Timer
  ( TimerState
  , HasTimerState(..)
  , timerRunner
  ) where

import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Time.Clock
import           Snap.Extension
import           Snap.Extension.Timer
import           Snap.Types

------------------------------------------------------------------------------
-- | Your application's state must include a 'TimerState' in order for your
-- application to be a 'MonadTimer'.
newtype TimerState = TimerState
    { _startTime :: UTCTime
    }


------------------------------------------------------------------------------
-- | For you appliaction's monad to be a 'MonadTimer', your application's
-- state needs to be an instance of 'HasTimerState'. Minimal complete
-- definition: 'getTimerState', 'setTimerState'.
class HasTimerState s where
    getTimerState :: s -> TimerState
    setTimerState :: TimerState -> s -> s


------------------------------------------------------------------------------
-- | The runner for 'TimerState'. No arguments are required.
timerRunner :: Runner TimerState
timerRunner = liftIO getCurrentTime >>= mkRunner . TimerState


------------------------------------------------------------------------------
instance RunnerState TimerState where
    extensionId = const "Snap.Extension.Timer"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()


------------------------------------------------------------------------------
instance HasTimerState s => MonadTimer (SnapExtend s) where
    startTime = fmap _startTime $ asks getTimerState


------------------------------------------------------------------------------
instance (MonadSnap m, HasTimerState s) => MonadTimer (ReaderT s m) where
    startTime = fmap _startTime $ asks getTimerState
