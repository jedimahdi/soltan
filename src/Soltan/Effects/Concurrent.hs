module Soltan.Effects.Concurrent where

import Pipes (Consumer', Producer')
import Pipes.Concurrent (Buffer, Input, Output)
import qualified Pipes.Concurrent
import Soltan.SocketApp (SocketApp)
import UnliftIO (MonadUnliftIO, async)
import qualified UnliftIO

class Monad m => Concurrent m where
  forkProcess :: m a -> m ()
  fromInput :: Input a -> Producer' a m ()
  toOutput :: Output a -> Consumer' a m ()
  spawn :: Buffer a -> m (Output a, Input a)
  finally :: m a -> m b -> m a

instance Concurrent SocketApp where
  forkProcess = void . async
  fromInput = Pipes.Concurrent.fromInput
  toOutput = Pipes.Concurrent.toOutput
  spawn = liftIO . Pipes.Concurrent.spawn
  finally = UnliftIO.finally
