module Soltan.Effects.Lobby where

import Soltan.Socket.Types (Table, TableName, Lobby)
import Soltan.Hokm (Game)
import Soltan.Data.Username (Username)

class Monad m => ManageLobby m where
  getTable :: TableName -> m (Maybe Table)
  getLobby :: m Lobby
  updateGame :: TableName -> Game -> m ()
  addSubscriber :: TableName -> Username -> m ()


      -- <| modifyTVar' serverStateTVar (#lobby . ix tableName . #subscribers <>~ [username])

withTable :: ManageLobby m => TableName -> m r -> (Table -> m r) -> m r
withTable tableName onFail app = getTable tableName >>= maybe onFail app
