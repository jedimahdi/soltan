module Soltan.Effects.Now where

import Chronos (Time)
import Soltan.SocketApp (SocketApp)
import qualified Chronos

class Monad m => Now m where
  now :: m Time

instance Now IO where
  now = Chronos.now

instance MonadIO m => Now (StateT s m) where
  now = liftIO Chronos.now

instance MonadIO m => Now (ReaderT s m) where
  now = liftIO Chronos.now

instance Now SocketApp where
  now = liftIO Chronos.now
