{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Server.Game where

import Control.Exception.Safe (MonadMask)
import qualified Network.WebSockets as WS
import Servant.API.WebSocket (WebSocket)
import Soltan.App.Lobby (Room (..), RoomStatus (..), SomeRoom)
import Soltan.App.Monad (App)
import qualified Soltan.Data.Game as Game
import Soltan.Data.Username (Username)
import Soltan.Effect.GameState (GameState)
import Soltan.Effect.Hub (MonadHub, sendJSON)
import Soltan.Effect.Id (MonadId, nextId)
import Soltan.Effect.Lobby (MonadLobby)
import qualified Soltan.Effect.Lobby as Lobby
import Soltan.Effect.Shuffle (MonadShuffle)
import qualified Soltan.Logger as Logger
import Soltan.Server.Prelude
import Soltan.Server.Socket (
  handleJoinGame,
  handleLeaveGame,
  withConnection,
 )

data Routes route = Routes
  { join :: route :- Capture "gameId" Game.Id :> Capture "username" Username :> WebSocket
  }
  deriving stock (Generic)

type Api = ToApi Routes

server :: Routes AppServer
server =
  Routes
    { join = joinHandler
    }

-- joinHandler :: ( MonadLobby m
--                , MonadHub m
--                , MonadMask m, GameState m
--                , MonadShuffle m
joinHandler :: Game.Id -> Username -> WS.Connection -> App ()
joinHandler gameId username conn = do
  Logger.debug <| "Connected to game " <> show gameId <> " user " <> show username
  withConnection conn username (handleLeaveGame gameId username) (handleJoinGame gameId username)
