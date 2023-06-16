module Soltan.Effects.Clients where

import Control.Lens (at, (?~))
import Soltan.Data.Has (Has, grab)
import Soltan.Socket.Types (Client, ServerState)
import Soltan.SocketApp (SocketApp)

class Monad m => ManageClients m where
  addClient :: Client -> m ()

type WithServerState env m = (MonadReader env m, Has (TVar ServerState) env, MonadIO m)

instance ManageClients SocketApp where
  addClient = addClientImpl

addClientImpl :: WithServerState env m => Client -> m ()
addClientImpl client = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#clients . at (client ^. #username) ?~ client)
