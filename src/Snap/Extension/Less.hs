{-# LANGUAGE OverloadedStrings #-}

{-|

This Snap extension makes it easy to use LessCSS stylesheets in your Snap
application. See http:\/\/lesscss.org\/. The operations you're interested in
are probably 'lessServe' and 'lessServeSingle'.

-}

module Snap.Extension.Less
  ( MonadLess(..)
  , HasLessState(..)
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
import           Snap.Types hiding (dir, path)
import           Snap.Util.FileServe
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.IO
import           System.Process


------------------------------------------------------------------------------
-- | The 'MonadLess' typeclass. It adds the operations 'lessServe' and
-- 'lessServeSingle' to your monad.
class MonadSnap m => MonadLess m where
    -- | Analogous to 'fileServe'. If the template specified in the request
    -- path is not found, it returns 'empty'.
    lessServe       :: m ()

    -- | Analogous to 'fileServeSingle'. If the given template is not found,
    -- this throws an error.
    lessServeSingle :: ByteString -> m ()


------------------------------------------------------------------------------
instance HasLessState s => MonadLess (SnapExtend s) where
    lessServe         = getSafePath >>= getStylesheet . U.fromString
                                    >>= maybe pass cssServe

    lessServeSingle p = getStylesheet p >>= maybe notFound cssServe
      where
        notFound = error $ "Stylesheet `" ++ U.toString p ++ "' not found"


------------------------------------------------------------------------------
-- | A helper function used in the implementation of 'lessServe' and
-- 'lessServeSingle'.
getStylesheet :: HasLessState s => ByteString -> SnapExtend s (Maybe ByteString)
getStylesheet n = do
    (LessState _ m) <- asks getLessState
    liftIO $ readMVar m >>= return . M.lookup n


------------------------------------------------------------------------------
-- | A helper function used in the implementation of 'lessServe' and
-- 'lessServeSingle'.
cssServe :: MonadSnap m => ByteString -> m ()
cssServe css = do
    modifyResponse $ setContentType "text/css; charset=utf-8"
                   . setContentLength (fromIntegral $ B.length css)
    writeBS css


------------------------------------------------------------------------------
class HasLessState s where
    getLessState :: s -> LessState
    setLessState :: LessState -> s -> s


------------------------------------------------------------------------------
-- | The internal state of any 'MonadLess'. This is more or less just a
-- mapping from the names of virtual @.css@ filess to their contents.
data LessState = LessState FilePath (MVar (Map ByteString ByteString))


------------------------------------------------------------------------------
instance RunnerState LessState where
    extensionId = const "Snap.Extension.Less"
    mkCleanup   = const $ return ()
    mkReload (LessState p m) = modifyMVar_ m $ const $ loadStylesheets p


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
-- | A version of 'readProcessExitCode' which operates on 'ByteString's. More
-- or less stolen from Heist.
readProcessWithExitCode'
    :: FilePath
    -> [String]
    -> ByteString
    -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode' cmd args input = do
    (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

    outM <- newEmptyMVar
    errM <- newEmptyMVar
    sync <- newEmptyMVar

    forkIO $ do
        out <- B.hGetContents outh
        putMVar outM out
        putMVar sync ()

    forkIO $ do
        err  <- B.hGetContents errh
        putMVar errM err
        putMVar sync ()

    when (not $ B.null input) $ B.hPutStr inh input >> hFlush inh

    hClose inh
    takeMVar sync
    takeMVar sync
    hClose outh

    exitCode <- waitForProcess pid
    out <- readMVar outM
    err <- readMVar errM
    return (exitCode, out, err)
