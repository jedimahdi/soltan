module Soltan.Server.Game where

import           Control.Exception.Safe (MonadMask)
import qualified Network.WebSockets     as WS
import           Servant.API.WebSocket  (WebSocket)
import           Soltan.App.Lobby       (Room (..), RoomStatus (..), SomeRoom)
import qualified Soltan.Data.Game       as Game
import           Soltan.Data.Username   (Username)
import           Soltan.Effect.Hub      (MonadHub)
import           Soltan.Effect.Id       (MonadId, nextId)
import qualified Soltan.Effect.Lobby    as Lobby
import           Soltan.Effect.Lobby    (MonadLobby)
import           Soltan.Server.Prelude
import           Soltan.Server.Socket   (withConnection)

data Routes route
  = Routes
      { join :: route :- Capture "gameId" Game.Id :> Capture "username" Username :> WebSocket
      }
  deriving stock (Generic)

type Api = ToApi Routes

server :: Routes AppServer
server = Routes
  { join = joinHandler
  }

joinHandler :: (MonadLobby m, MonadHub WS.Connection m, MonadMask m) => Game.Id -> Username -> WS.Connection -> m ()
joinHandler gameId username conn = do
  withConnection conn username pass pass
  pass

