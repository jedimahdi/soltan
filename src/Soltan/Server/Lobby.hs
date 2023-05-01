{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Server.Lobby where

import Soltan.App.Lobby (Room (..), RoomStatus (..), SomeRoom)
import qualified Soltan.Data.Game as Game
import Soltan.Data.Game.Card (fullDeck)
import Soltan.Data.UserGame (UserGame, mkUserGame)
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Username as Username
import Soltan.Effect.Id (MonadId, nextId)
import Soltan.Effect.Lobby (MonadLobby)
import qualified Soltan.Effect.Lobby as Lobby
import Soltan.Server.Prelude

data CreateRoomRequest = CreateRoomRequest
  { name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Routes route = Routes
  { getAll :: route :- Get '[JSON] [SomeRoom]
  , create :: route :- "room" :> ReqBody '[JSON] CreateRoomRequest :> Post '[JSON] NoContent
  , randomGame :: route :- "gg" :> Get '[JSON] UserGame
  }
  deriving stock (Generic)

type Api = ToApi Routes

server :: Routes AppServer
server =
  Routes
    { getAll = getAllHandler
    , create = createHandler
    , randomGame = randomGameHandler
    }

randomGameHandler :: (Monad m, MonadId m) => m UserGame
randomGameHandler = do
  id <- nextId
  let username1 = Username.UnsafeMk "zoro"
  let username2 = Username.UnsafeMk "luffy"
  let username3 = Username.UnsafeMk "robin"
  let username4 = Username.UnsafeMk "chopper"
  let game = Game.mk id [username1, username2, username3, username4] fullDeck

  case game of
    Left e -> error "fail to create game"
    Right g -> do
      let usergame = mkUserGame username1 g
      case usergame of
        Nothing -> error "fail to make user game"
        Just u -> pure u

createHandler :: (MonadLobby m, MonadId m, MonadIO m) => CreateRoomRequest -> m NoContent
createHandler CreateRoomRequest{..} = do
  id <- nextId
  let room :: Room 'Open = Room id name [] Open
  Lobby.addRoom room
  putStrLn $ "[Create Room] " <> show room
  pure NoContent

getAllHandler :: MonadLobby m => m [SomeRoom]
getAllHandler = Lobby.getRooms
