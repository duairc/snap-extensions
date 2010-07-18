{-

This module is for various utility functions which are used by one or more
Snap Extensions.

-}

module Snap.Extension.Utils
  ( readProcessWithExitCode'
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           System.Exit
import           System.IO
import           System.Process

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
