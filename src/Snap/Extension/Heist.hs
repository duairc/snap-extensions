{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

'Snap.Extension.Heist' is a Snap extension that makes it easy to use Heist
templates with your Snap application. It extends your monad with the
operations 'heistServe', 'heistServeSingle', 'heistLocal' and 'render'.

It is important to note that the 'HeistState' type exported is of kind
@(* -> *) -> *@, as opposed to @*@ which is the norm for @-State@ types. The
type argument you should give to 'HeistState' is your application's monad, or
more specifically, @m@, where @Splice m@ is the type of the splices you wish
to bind to 'HeistState'. To bind splices to the 'HeistState', use
'heistLocal (bindSplices mySplices)'.

This extension does not depend on any other extensions.

-}

module Snap.Extension.Heist
  ( MonadHeist(..)
  , heistServe
  , heistServeSingle
  , HasHeistState(..)
  , HeistState
  , heistRunner
  ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as U
import           Snap.Extension
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


------------------------------------------------------------------------------
-- | The 'MonadHeist' typeclass. It adds the operations 'render',
-- 'heistLocal', 'heistServe' and 'heistServeSingle' to your monad.
class (MonadSnap m, Monad n) => MonadHeist m n | m -> n where
    -- | Renders a template as text\/html. If the given template is not found,
    -- this returns 'empty'.
    render     :: ByteString -> m ()

    -- | Runs an action with a modified 'TemplateState'. You might want to use
    -- this if you had a set of splices which were customised for a specific
    -- action. To do that you would do:
    --
    -- > heistLocal (bindSplices mySplices) $ render "myTemplate"
    heistLocal :: (TemplateState n -> TemplateState n) -> m a -> m a


------------------------------------------------------------------------------
-- | Analogous to 'fileServe'. If the template specified in the request path
-- is not found, it returns 'empty'.
heistServe :: MonadHeist m n => m ()
heistServe = render . U.fromString =<< getSafePath


------------------------------------------------------------------------------
-- | Analogous to 'fileServeSingle'. If the given template is not found, this
-- throws an error.
heistServeSingle :: MonadHeist m n => ByteString -> m ()
heistServeSingle t = render t <|>
                     error ("Template \"" ++ U.toString t ++ "\" not found.")


------------------------------------------------------------------------------
instance
    HasHeistState s (SnapExtend s) => MonadHeist (SnapExtend s) (SnapExtend s)
  where
    render t     = do
        (HeistState _ _ tsMVar _ modifier) <- asks getHeistState
        ts <- liftIO $ fmap modifier $ readMVar tsMVar
        mt <- renderTemplate ts t
        flip (maybe pass) mt $ \html -> do
            modifyResponse $ setContentType "text/html; charset=utf-8"
                           . setContentLength (fromIntegral $ B.length html)
            writeBS html

    heistLocal f = local $ modifyHeistState $ \s ->
        s { _modifier = f . _modifier s }


------------------------------------------------------------------------------
instance HasHeistState s m => MonadHeist (ReaderT s m) m where
    render t     = ReaderT $ \s -> do
        let (HeistState _ _ tsMVar _ modifier) = getHeistState s
        ts <- liftIO $ fmap modifier $ readMVar tsMVar
        mt <- renderTemplate ts t
        flip (maybe pass) mt $ \html -> do
            writeBS html
            modifyResponse
                $ setContentType "text/html; charset=utf-8"
                . setContentLength (fromIntegral $ B.length html)

    heistLocal f = local $ modifyHeistState $ \s ->
        s { _modifier = f . _modifier s }


------------------------------------------------------------------------------
class MonadSnap m => HasHeistState s m | s -> m where
    getHeistState :: s -> HeistState m
    setHeistState :: HeistState m -> s -> s
    modifyHeistState :: (HeistState m -> HeistState m) -> s -> s
    modifyHeistState f s = setHeistState (f $ getHeistState s) s


------------------------------------------------------------------------------
-- | HeistState is the internal state of any 'MonadHeist'. It stores the
-- 'TemplateState' and the 'StaticTagState' and enough additional information
-- to support 'heistRunner''s reload action which flushes the 'StaticTagState'
-- and reloads all the templates in the 'TemplateState' from disk.
data MonadSnap m => HeistState m = HeistState
    { _path     :: FilePath
    , _origTs   :: TemplateState m
    , _tsMVar   :: MVar (TemplateState m)
    , _sts      :: StaticTagState
    , _modifier :: TemplateState m -> TemplateState m
    }


------------------------------------------------------------------------------
instance MonadSnap m => RunnerState (HeistState m) where
    extensionId = const "Snap.Extension.Heist"
    mkCleanup   = const $ return ()
    mkReload (HeistState path origTs tsMVar sts _) = do
        clearStaticTagCache $ sts
        either error (modifyMVar_ tsMVar . const . return) =<<
            loadTemplates path origTs


------------------------------------------------------------------------------
-- | The runner for the Heist extension. It takes a path to a template
-- directory containing @.tpl@ files.
heistRunner :: MonadSnap m => FilePath -> Runner (HeistState m)
heistRunner path = do
    heistState <- liftIO $ do
        (origTs,sts) <- bindStaticTag emptyTemplateState
        ets <- loadTemplates path origTs
        flip (either error) ets $ \ts -> do
            tsMVar <- newMVar ts
            return $ HeistState path origTs tsMVar sts id
    mkRunner heistState
