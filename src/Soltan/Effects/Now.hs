module Soltan.Effects.Now where

import Chronos (Time)
import qualified Chronos
import Pipes (Proxy)
import Soltan.SocketApp (SocketApp)
import Prelude hiding (Proxy)

class Monad m => Now m where
  now :: m Time

instance Now IO where
  now = Chronos.now

instance Now m => Now (StateT s m) where
  now = lift now

instance Now m => Now (ReaderT r m) where
  now = lift now

instance Now m => Now (ExceptT e m) where
  now = lift now

instance Now m => Now (Proxy a' a b' b m) where
  now = lift now

instance Now SocketApp where
  now = liftIO Chronos.now
