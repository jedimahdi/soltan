module Soltan.App.Channel where

import Control.Lens (Lens')
import Data.Generics.Labels ()
import Soltan.App.Types
import Soltan.Data.Username (Username)
import UnliftIO.STM (TBQueue, TQueue, newTBQueueIO, readTBQueue, writeTBQueue, writeTQueue)

type Channel a = TBQueue a

newChannel :: (MonadIO m) => m (Channel a)
newChannel = newTBQueueIO 10

writeChannel :: (MonadIO m) => Channel a -> a -> m ()
writeChannel channel a = atomically $ writeTBQueue channel a

readChannel :: (MonadIO m) => Channel a -> m a
readChannel channel = atomically $ readTBQueue channel
