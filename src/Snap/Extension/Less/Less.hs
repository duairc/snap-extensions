{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Less.Less' is an implementation of the 'MonadLess'
interface defined in 'Snap.Extension.Less'.

As always, to use, add 'LessState' to your application's state, along with an
instance of 'HasLessState' for your application's state, making sure to use a
'lessRunner' in your application's 'Runner', and then you're ready to go.

This implementation does not require that your application's monad implement
interfaces from any other Snap Extension.

-}

module Snap.Extension.Less.Less
  ( HasLessState(..)
  , LessState
  , lessRunner
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import           Snap.Extension
import           Snap.Extension.Less
import           Snap.Extension.Utils
import           Snap.Types hiding (dir, path)
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.IO


------------------------------------------------------------------------------
-- | Your application's state must include a 'LessState' in order for your
-- application to be a 'MonadLess'.
data LessState = LessState
    { _dir     :: FilePath
    , _mapping :: MVar (Map ByteString ByteString)
    }


------------------------------------------------------------------------------
-- | For you appliaction's monad to be a 'MonadLess', your application's state
-- needs to be an instance of 'HasLessState'. Minimal complete definition:
-- 'getLessState', 'setLessState'.
class HasLessState s where
    getLessState :: s -> LessState
    setLessState :: LessState -> s -> s


------------------------------------------------------------------------------
instance RunnerState LessState where
    extensionId = const "Snap.Extension.Less"
    mkCleanup   = const $ return ()
    mkReload (LessState d m) = modifyMVar_ m $ const $ loadStylesheets d


------------------------------------------------------------------------------
-- | The Runner for the Less extension. It takes a path to a stylesheet
-- directory containing @.less@ files.
lessRunner :: FilePath -> Runner LessState
lessRunner path = mkRunner =<< (liftIO $
    loadStylesheets path >>= newMVar >>= return . LessState path)


------------------------------------------------------------------------------
-- | Locates the 'lessc' executable.
findLessc :: IO FilePath
findLessc = do
    findExecutable "lessc" >>= maybe (findM exec gems) (return . Just) >>=
        maybe (error "Could not find executable `lessc'") return
  where
    gems    = ["/var/lib/gems/1.9.1/bin/lessc", "/var/lib/gems/1.8/bin/lessc"]
    findM p = fmap listToMaybe . filterM p
    exec f  = do
        exists <- doesFileExist f
        if exists then getPermissions f >>= return . executable
                  else return False


------------------------------------------------------------------------------
-- | Given the path to a @.less@ file, this returns its contents processed by
-- @lessc@.
loadStylesheet :: FilePath -> IO ByteString
loadStylesheet p = do
    l <- findLessc
    (exitCode, out, err) <- readProcessWithExitCode' l [p, "/dev/stdout"] ""
    when (exitCode /= ExitSuccess) $ error $ p ++ ": " ++ U.toString err
    return out


------------------------------------------------------------------------------
-- | Given the path to a directory containing @.less@ files, this returns a
-- map from the names of those @.less@ files with the @.less@ replaced with
-- @.css@ to the contents of those files after being processed with the
-- @lessc@ command.
loadStylesheets :: FilePath -> IO (Map ByteString ByteString)
loadStylesheets path = readDirectoryWith reader path >>=
    return . M.fromList . map fromJust . filter isJust . F.toList . free
  where
    reader file = if ".less" `isSuffixOf` file then do
        stylesheet <- loadStylesheet file
        return $ Just $ (U.fromString $ cssify file, stylesheet)
      else
        return Nothing
    cssify p = (drop (length path + 1) $ take (length p - 5) p) ++ ".css"


------------------------------------------------------------------------------
instance HasLessState s => MonadLess (SnapExtend s) where
    lessServe = getRequest >>= getStylesheet . rqPathInfo >>= maybe pass serve

    lessServeSingle p = getStylesheet p >>=
        maybe (error $ "Stylesheet " ++ show p ++ " not found") serve


------------------------------------------------------------------------------
instance (MonadSnap m, HasLessState s) => MonadLess (ReaderT s m) where
    lessServe = getRequest >>= getStylesheet . rqPathInfo >>= maybe pass serve

    lessServeSingle p = getStylesheet p >>=
        maybe (error $ "Stylesheet " ++ show p ++ " not found") serve


------------------------------------------------------------------------------
-- | A helper function used in the implementation of 'lessServe' and
-- 'lessServeSingle'.
getStylesheet :: (MonadSnap m, HasLessState s, MonadReader s m)
              => ByteString -> m (Maybe ByteString)
getStylesheet stylesheet = do
    (LessState _ mapping) <- asks getLessState
    liftIO $ readMVar mapping >>= return . M.lookup stylesheet


------------------------------------------------------------------------------
-- | A helper function used in the implementation of 'lessServe' and
-- 'lessServeSingle'.
serve :: MonadSnap m => ByteString -> m ()
serve css = do
    modifyResponse $ setContentType "text/css; charset=utf-8"
    modifyResponse $ setContentLength (fromIntegral $ B.length css)
    writeBS css
