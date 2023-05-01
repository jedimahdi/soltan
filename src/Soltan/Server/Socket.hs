module Soltan.Server.Socket where

import Control.Exception.Safe (MonadMask, finally)
import Control.Lens (at, to)
import Data.Constraint (Dict (..))
import Data.Singletons (withSing)
import qualified Network.WebSockets as WS
import Soltan.App.Lobby (Room, RoomStatus (..))
import Soltan.Data.Game (Game, PlayedCard, playersL, turnL)
import qualified Soltan.Data.Game as Game
import Soltan.Data.Game.Card (Card)
import Soltan.Data.UserGame (UserGame, mkUserGame)
import Soltan.Data.Username
import Soltan.Effect.GameState (GameState)
import qualified Soltan.Effect.GameState as GameState
import Soltan.Effect.Hub (MonadHub)
import qualified Soltan.Effect.Hub as Hub
import Soltan.Effect.Lobby (MonadLobby)
import qualified Soltan.Effect.Lobby as Lobby
import Soltan.Effect.Shuffle (MonadShuffle, mkShuffledFullDeck)
import Soltan.Logger (WithLog)
import qualified Soltan.Logger as Logger

withConnection :: MonadHub m => MonadMask m => Hub.Connection m -> Username -> m () -> m () -> m ()
withConnection conn username onDisconnect onConnect = flip finally handleDisconnect do
  Hub.subscribe username conn
  onConnect
  Hub.keepConnectionAlive conn
 where
  handleDisconnect = do
    Hub.unsubscribe username
    onDisconnect

data Message
  = LeftGame Username
  | JoinedBack Username
  | Joined [Username]
  | Game UserGame
  deriving stock (Generic)
  deriving anyclass (ToJSON)

handleJoinGame ::
  forall m e.
  ( GameState m
  , MonadLobby m
  , MonadShuffle m
  , MonadHub m
  , MonadIO m
  , WithLog e m
  ) =>
  Game.Id ->
  Username ->
  m ()
handleJoinGame gameId username = do
  Lobby.withRoom gameId openRoom fullRoom
 where
  fullRoom :: Room 'Full -> m ()
  fullRoom room = do
    Logger.debug <| "Join full room" <> show room
    GameState.findGameById gameId
      >>= maybe
        gameNotFound
        ( \game -> do
            Logger.debug <| "Join game" <> show game
            Hub.sendJSON (Game <$> mkUserGame username game) username
        )
    Lobby.broadcastRoom' (JoinedBack username) gameId

  openRoom :: Room 'Open -> m ()
  openRoom room = do
    Logger.debug <| "Join open room " <> show room
    Lobby.joinRoom username room >>= either (const pass) startGame
    Hub.sendJSON' (Joined (room ^. #users)) username

  startGame :: Room 'Full -> m ()
  startGame room = do
    shuffledDeck <- mkShuffledFullDeck
    let roomId = room ^. #id
    let game = Game.mk (room ^. #id) (room ^. #users) shuffledDeck
    let x = rightToMaybe game >>= mkUserGame username
    let createGame g = do
          Lobby.broadcastRoom (`mkUserGame` g) roomId
          GameState.addGame g

    Logger.debug <| "Start game (create new game)" <> show game
    either createGameError createGame game

  gameNotFound :: m ()
  gameNotFound = Logger.warning <| "Game not found " <> show gameId <> show username

  createGameError :: Game.Error -> m ()
  createGameError err = Logger.warning <| "Error in creating game " <> show gameId <> show err

handleLeaveGame ::
  forall m e.
  ( GameState m
  , MonadLobby m
  , MonadHub m
  , WithLog e m
  , MonadIO m
  ) =>
  Game.Id ->
  Username ->
  m ()
handleLeaveGame gameId username = Lobby.withRoom gameId openRoom fullRoom
 where
  fullRoom :: Room 'Full -> m ()
  fullRoom room = do
    Logger.debug <| "Leave full room " <> show room
    Lobby.broadcastRoom' (LeftGame username) gameId

  openRoom :: Room 'Open -> m ()
  openRoom room = do
    Logger.debug <| "Leave open room " <> show room
    void <| Lobby.leaveRoom username room
