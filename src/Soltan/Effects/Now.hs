module Soltan.Effects.Now where

import Chronos (Time)
import Soltan.SocketApp (SocketApp)
import qualified Chronos

class Monad m => Now m where
  now :: m Time

instance Now SocketApp where
  now = liftIO Chronos.now
