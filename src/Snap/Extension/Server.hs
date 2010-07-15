{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

This module provides replacements for the 'httpServe' and 'quickHttpServe'
functions exported by 'Snap.Http.Server'. By taking a 'Runner' as an argument,
these functions simplify the glue code that is needed to use Snap Extensions.
In particular, 'Snap.Extension.Server.Hint' provides function with identical
type signatures to the ones exported by this module, but which dynamically
reload their code on each request. See the README for details.

-}

#ifdef HINT
module Snap.Extension.Server.Hint
#else
module Snap.Extension.Server
#endif
  ( ConfigExtend
  , httpServe
  , quickHttpServe
  , defaultConfig
  , getReloadHandler
  , setReloadHandler
  , module Snap.Http.Server.Config
  ) where

import           Control.Applicative
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as U
import           Data.Maybe
import           Data.Monoid
import           Prelude hiding (catch)
import           Snap.Extension
#ifdef HINT
import           Snap.Loader.Hint
#else
import           Snap.Loader.Static
#endif
import           Snap.Http.Server (simpleHttpServe, setUnicodeLocale)
import qualified Snap.Http.Server.Config as C
import           Snap.Http.Server.Config hiding ( defaultConfig
                                                , completeConfig
                                                , getOther
                                                , setOther
                                                )
import           Snap.Util.GZip
import           Snap.Types
import           System.IO


------------------------------------------------------------------------------
-- | 'ConfigExtend' is similar to the 'Config' exported by 'Snap.Http.Server',
-- but is augmented with a @reloadHandler@ field which can be accessed using
-- 'getReloadHandler' and 'setReloadHandler'.
type ConfigExtend s = Config
    (SnapExtend s) (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())


------------------------------------------------------------------------------
getReloadHandler :: ConfigExtend s -> Maybe
                      (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
getReloadHandler = C.getOther


------------------------------------------------------------------------------
setReloadHandler :: (IO [(ByteString, Maybe ByteString)] -> SnapExtend s ())
                 -> ConfigExtend s
                 -> ConfigExtend s
setReloadHandler = C.setOther


------------------------------------------------------------------------------
-- | These are the default values for all the fields in 'ConfigExtend'.
--
-- > hostname      = "localhost"
-- > address       = "0.0.0.0"
-- > port          = 8000
-- > accessLog     = "log/access.log"
-- > errorLog      = "log/error.log"
-- > locale        = "en_US"
-- > compression   = True
-- > verbose       = True
-- > errorHandler  = prints the error message
-- > reloadHandler = prints the result of each reload handler (error/success)
--
defaultConfig :: ConfigExtend s
defaultConfig = setReloadHandler handler C.defaultConfig
  where
    handler = path "admin/reload" . defaultReloadHandler


------------------------------------------------------------------------------
-- | Completes a partial 'Config' by filling in the unspecified values with
-- the default values from 'defaultConfig'.
completeConfig :: ConfigExtend s -> ConfigExtend s
completeConfig = mappend defaultConfig


------------------------------------------------------------------------------
-- | Starts serving HTTP requests using the given handler, with settings from
-- the 'ConfigExtend' passed in. This function never returns; to shut down
-- the HTTP server, kill the controlling thread.
httpServe :: ConfigExtend s
          -- ^ Any configuration options which override the defaults
          -> Runner s
          -- ^ The 'Runner' function for the application's monad
          -> SnapExtend s ()
          -- ^ The application to be served
          -> IO ()
httpServe config runner handler = do
    (state, mkCleanup, mkSnap) <-
        runRunnerHint verbose runner (catch500 handler) reloader
    (cleanup, snap) <-
        $(loadSnapTH 'state 'mkCleanup 'mkSnap)
    let site = compress $ snap
    output $ concat ["Listening on ", U.toString address, ":", show port]
    _   <- try $ serve $ site :: IO (Either SomeException ())
    cleanup
    output "\nShutting down..."
  where
    handle   :: SomeException -> IO ()
    handle e = print e
    conf     = completeConfig config
    verbose  = fromJust $ getVerbose conf
    output   = when verbose . hPutStrLn stderr
    address  = fromJust $ getAddress conf
    port     = fromJust $ getPort conf
    reloader = fromJust $ getReloadHandler conf
    compress = if fromJust $ getCompression conf then withCompression else id
    catch500 = flip catch $ fromJust $ getErrorHandler conf
    serve    = simpleHttpServe config


------------------------------------------------------------------------------
-- | Starts serving HTTP using the given handler. The configuration is read
-- from the options given on the command-line, as returned by
-- 'commandLineConfig'.
quickHttpServe :: Runner s
               -- ^ The 'Runner' function for the application's monad
               -> SnapExtend s ()
               -- ^ The application to be served
               -> IO ()
quickHttpServe r m = commandLineConfig emptyConfig >>= \c -> httpServe c r m
