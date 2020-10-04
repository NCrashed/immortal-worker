{-|
Module      : Control.Immortal.Worker
Description : Immortal thread with logging and restart on exceptions.
Copyright   : (c) Anton Gushcha (ncrashed), 2020
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

Here is a longer description of this module, containing some
commentary with @some markup@.

Typical usage:

@
worker "supervisor" $ const $ forever $ do
  logInfoN "Supervisor started"
  let subworkers = [
          subworker1
        , subworker2
        ]
  traverse_ (isolate_ "subworker") subworkers
  liftIO $ threadDelay 10_000_000
@

-}
module Control.Immortal.Worker(
    workerWith
  , worker
  , isolate
  , isolate_
  ) where

import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Exception.Safe (SomeException, catchDeep)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Text (pack)

import qualified Control.Immortal as I

-- | Start immortal worker that logs on exceptions and restarts.
--
-- Note that action is not looped implicitly. Add 'Control.Monad.forever' into action
-- manually to achive this.
workerWith :: (MonadUnliftIO m, MonadLogger m)
  => (String -> SomeException -> m ()) -- ^ Action to perform before worker restart
  -> String -- ^ Worker label for thred
  -> (I.Thread -> m ()) -- ^ Worker action (no looping is added)
  -> m I.Thread
workerWith logthem lbl f = I.createWithLabel lbl $ \thread ->
  I.onUnexpectedFinish thread (either (logthem lbl) (const $ pure ())) (f thread)

-- | Helper that starts new immortal thread with logging of errors
worker :: (MonadUnliftIO m, MonadLogger m) => String -> (I.Thread -> m ()) -> m I.Thread
worker = workerWith $ \lbl e -> do
  logErrorN $ "Worker " <> pack lbl <> " exit with: " <> (pack . show) e
  liftIO $ threadDelay 1000000

-- | If computation fails, print log and return default value.
isolate :: (MonadUnliftIO m, MonadLogger m, NFData a) => String -> a -> m a -> m a
isolate title a0 ma = do
  run <- askRunInIO
  liftIO $ catchDeep (run ma) $ \(e :: SomeException) -> run $ do
    logErrorN $ "Isolated action " <> pack title <> " failed: " <> (pack . show) e
    pure a0

-- | Same as `isolate` but returns empty tuple
isolate_ :: (MonadUnliftIO m, MonadLogger m) => String -> m () -> m ()
isolate_ lbl = isolate lbl ()
