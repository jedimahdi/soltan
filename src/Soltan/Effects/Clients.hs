module Soltan.Effects.Clients where

import Control.Lens (at, (?~))
import Soltan.Data.Has (Has, grab)
import Soltan.Data.Username (Username)
import Soltan.Socket.Types (ServerState)
import Soltan.SocketApp (SocketApp)
import Soltan.Effects.Stream (MonadStream)
import qualified Soltan.Effects.Stream as Stream
import Soltan.Game.Types (MsgOut, MsgIn, Client)

type ManageClients m = (AcquireClients m, UpdateClients m, ClientStreams m)

class Monad m => AcquireClients m where
  getClients :: m (Map Username (Client m))

instance AcquireClients SocketApp where
  getClients = getClientsImpl

class AcquireClients m => UpdateClients m where
  addClient :: Client m -> m ()
  removeClient :: Username -> m ()

instance UpdateClients SocketApp where
  addClient = addClientImpl
  removeClient = removeClientImpl

class MonadStream m => ClientStreams m where
  fromUserSendStream :: Stream.Stream m MsgOut -> Stream.Producer m MsgOut
  toUserSendStream :: Stream.Stream m MsgOut -> Stream.Consumer m MsgOut
  fromUserRecieveStream :: Stream.Stream m MsgIn -> Stream.Producer m MsgIn
  toUserRecieveStream :: Stream.Stream m MsgIn -> Stream.Consumer m MsgIn

type WithServerState env m = (MonadReader env m, Has (TVar ServerState) env, MonadIO m)

getClientsImpl :: WithServerState env m => m (Map Username (Client m))
getClientsImpl = do
  serverStateTVar <- grab @(TVar ServerState)
  serverState <- readTVarIO serverStateTVar
  pure <| serverState ^. #clients

addClientImpl :: WithServerState env m => Client m -> m ()
addClientImpl client = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#clients . at (client ^. #username) ?~ client)

removeClientImpl :: WithServerState env m => Username -> m ()
removeClientImpl username = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#clients . at username .~ Nothing)
