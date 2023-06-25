module Soltan.Effects.Random where

import Pipes (Proxy)
import Soltan.SocketApp (SocketApp)
import System.Random (StdGen, getStdGen)
import Prelude hiding (Proxy)

class Monad m => MonadRandom m where
  generateStdGen :: m StdGen

instance MonadRandom IO where
  generateStdGen = getStdGen

instance MonadRandom m => MonadRandom (StateT s m) where
  generateStdGen = lift generateStdGen

instance MonadRandom m => MonadRandom (ReaderT r m) where
  generateStdGen = lift generateStdGen

instance MonadRandom m => MonadRandom (ExceptT e m) where
  generateStdGen = lift generateStdGen

instance MonadRandom m => MonadRandom (Proxy a' a b' b m) where
  generateStdGen = lift generateStdGen

instance MonadRandom SocketApp where
  generateStdGen = getStdGen
