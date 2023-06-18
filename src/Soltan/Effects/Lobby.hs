module Soltan.Effects.Lobby where

import Control.Lens (ix, (<>~), (^?))
import Soltan.Data.Has (Has, grab)
import Soltan.Data.Username (Username)
import Soltan.Hokm (Game)
import Soltan.Socket.Types (Lobby, ServerState, Table, TableName)
import Soltan.SocketApp (SocketApp)
import UnliftIO.STM (modifyTVar)

class Monad m => AcquireLobby m where
  getTable :: TableName -> m (Maybe Table)
  getLobby :: m Lobby

class AcquireLobby m => UpdateLobby m where
  updateGame :: TableName -> Game -> m ()
  addSubscriber :: TableName -> Username -> m ()

type ManageLobby m = (AcquireLobby m, UpdateLobby m)

withTable :: AcquireLobby m => TableName -> m r -> (Table -> m r) -> m r
withTable tableName onFail app = getTable tableName >>= maybe onFail app

instance AcquireLobby SocketApp where
  getTable = getTableImpl
  getLobby = getLobbyImpl

instance UpdateLobby SocketApp where
  updateGame = updateGameImpl
  addSubscriber = addSubscriberImpl

type WithServerState env m = (MonadReader env m, Has (TVar ServerState) env, MonadIO m)

getTableImpl :: WithServerState env m => TableName -> m (Maybe Table)
getTableImpl tableName = do
  serverStateTVar <- grab @(TVar ServerState)
  serverState <- readTVarIO serverStateTVar
  pure <| serverState ^? #lobby . ix tableName

getLobbyImpl :: WithServerState env m => m Lobby
getLobbyImpl = do
  serverStateTVar <- grab @(TVar ServerState)
  serverState <- readTVarIO serverStateTVar
  pure <| serverState ^. #lobby

updateGameImpl :: WithServerState env m => TableName -> Game -> m ()
updateGameImpl tableName game = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#lobby . ix tableName . #game .~ game)

addSubscriberImpl :: WithServerState env m => TableName -> Username -> m ()
addSubscriberImpl tableName username = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| modifyTVar' serverStateTVar (#lobby . ix tableName . #subscribers <>~ [username])
