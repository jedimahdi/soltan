module Soltan.Effects.Random where

import Soltan.SocketApp (SocketApp)
import System.Random (StdGen, getStdGen)

class Monad m => MonadRandom m where
  generateStdGen :: m StdGen

instance MonadRandom SocketApp where
  generateStdGen = getStdGen

instance MonadRandom IO where
  generateStdGen = getStdGen

instance MonadIO m => MonadRandom (StateT s m) where
  generateStdGen = getStdGen

instance MonadIO m => MonadRandom (ReaderT r m) where
  generateStdGen = getStdGen

instance MonadRandom m => MonadRandom (ExceptT e m) where
  generateStdGen = lift generateStdGen
