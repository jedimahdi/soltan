module Soltan.Effects.Random where

import Soltan.SocketApp (SocketApp)
import System.Random (StdGen, getStdGen)

class Monad m => MonadRandom m where
  generateStdGen :: m StdGen

instance MonadRandom SocketApp where
  generateStdGen = getStdGen
