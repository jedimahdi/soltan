module Soltan.Effects.Clients where

import Control.Lens (at, (?~))
import Soltan.Data.Has (Has, grab)
import Soltan.Data.Username (Username)
import Soltan.Socket.Types (Client, ServerState)
import Soltan.SocketApp (SocketApp)

type ManageClients m = (AcquireClients m, UpdateClients m)

class Monad m => AcquireClients m where
  getClients :: m (Map Username Client)

instance AcquireClients SocketApp where
  getClients = getClientsImpl

class AcquireClients m => UpdateClients m where
  addClient :: Client -> m ()
  removeClient :: Username -> m ()

instance UpdateClients SocketApp where
  addClient = addClientImpl
  removeClient = removeClientImpl

type WithServerState env m = (MonadReader env m, Has (TVar ServerState) env, MonadIO m)

getClientsImpl :: WithServerState env m => m (Map Username Client)
getClientsImpl = do
  serverStateTVar <- grab @(TVar ServerState)
  serverState <- readTVarIO serverStateTVar
  pure <| serverState ^. #clients

addClientImpl :: WithServerState env m => Client -> m ()
addClientImpl client = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#clients . at (client ^. #username) ?~ client)

removeClientImpl :: WithServerState env m => Username -> m ()
removeClientImpl username = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#clients . at username .~ Nothing)
