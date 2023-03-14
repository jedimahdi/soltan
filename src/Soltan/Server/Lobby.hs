{-# LANGUAGE DeriveAnyClass #-}
module Soltan.Server.Lobby where

import           Soltan.App.Lobby      (Room (..), RoomStatus (..), SomeRoom)
import qualified Soltan.Data.Game      as Game
import           Soltan.Data.Username  (Username)
import           Soltan.Effect.Id      (MonadId, nextId)
import qualified Soltan.Effect.Lobby   as Lobby
import           Soltan.Effect.Lobby   (MonadLobby)
import           Soltan.Server.Prelude

data CreateRoomRequest
  = CreateRoomRequest
      { name :: Text
      }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Routes route
  = Routes
      { getAll :: route :- Get '[JSON] [SomeRoom]
      , create :: route :- "room" :> ReqBody '[JSON] CreateRoomRequest :> Post '[JSON] NoContent
      }
  deriving stock (Generic)

type Api = ToApi Routes

server :: Routes AppServer
server = Routes
  { getAll = getAllHandler
  , create = createHandler
  }

createHandler :: (MonadLobby m, MonadId m) => CreateRoomRequest -> m NoContent
createHandler CreateRoomRequest {..} = do
  id <- nextId
  let room :: Room 'Open = Room id name [] Open
  Lobby.addRoom room
  pure NoContent

getAllHandler :: MonadLobby m => m [SomeRoom]
getAllHandler = Lobby.getRooms
