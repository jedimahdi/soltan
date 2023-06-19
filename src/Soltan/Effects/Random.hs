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

instance MonadIO m => MonadRandom (ReaderT s m) where
  generateStdGen = getStdGen
