{-# LANGUAGE FunctionalDependencies #-}
module Soltan.Effect.Hub where

import Soltan.Data.Username (Username)
import qualified Network.WebSockets as WS

class Monad m => MonadHub c m | m -> c where
  subscribe   :: c -> Username -> m ()
  unsubscribe :: Username -> m ()
  sendMessage :: ByteString -> Username -> m ()
  stayAlive :: m ()
