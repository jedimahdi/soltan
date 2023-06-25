module Soltan.Effects.Concurrent where

import Pipes (Consumer', Producer', Proxy)
import Pipes.Concurrent (Buffer, Input, Output)
import qualified Pipes.Concurrent
import Soltan.SocketApp (SocketApp)
import UnliftIO (MonadUnliftIO, async)
import qualified UnliftIO
import qualified UnliftIO.Concurrent as UnliftIO
import Prelude hiding (Proxy)

class Monad m => Concurrent m where
  forkProcess :: m a -> m ()
  fromInput :: Input a -> Producer' a m ()
  toOutput :: Output a -> Consumer' a m ()
  spawn :: Buffer a -> m (Output a, Input a)
  finally :: m a -> m b -> m a
  threadDelay :: Int -> m ()

instance Concurrent SocketApp where
  forkProcess = void . async
  fromInput = Pipes.Concurrent.fromInput
  toOutput = Pipes.Concurrent.toOutput
  spawn = liftIO . Pipes.Concurrent.spawn
  finally = UnliftIO.finally
  threadDelay = UnliftIO.threadDelay
