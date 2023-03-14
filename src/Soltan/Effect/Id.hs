module Soltan.Effect.Id where

import           Data.UUID.V4     (nextRandom)
import           Soltan.App.Monad (App)

class MonadId m where
  nextId :: m UUID

instance MonadId App where
  nextId = nextIdImpl

nextIdImpl :: MonadIO m => m UUID
nextIdImpl = liftIO nextRandom
