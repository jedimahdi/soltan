module Soltan.Server.Socket where

import Control.Exception.Safe (MonadMask, finally)
import Control.Lens (to)
import Data.Constraint (Dict (..))
import Data.Singletons (withSing)
import qualified Network.WebSockets as WS
import Soltan.App.Lobby (Room, RoomStatus (..))
import qualified Soltan.Data.Game as Game
import Soltan.Data.Game (Game)
import Soltan.Data.Username
import qualified Soltan.Effect.GameState as GameState
import Soltan.Effect.GameState (GameState)
import qualified Soltan.Effect.Hub as Hub
import Soltan.Effect.Hub (MonadHub)
import qualified Soltan.Effect.Lobby as Lobby
import Soltan.Effect.Lobby (MonadLobby)
import Soltan.Effect.Shuffle (MonadShuffle, mkShuffledFullDeck)

withConnection :: MonadHub c m => MonadMask m => c -> Username -> m () -> m () -> m ()
withConnection conn username onDisconnect onConnect = flip finally handleDisconnect do
  Hub.subscribe username conn
  onConnect
  Hub.keepConnectionAlive conn
  where
    handleDisconnect = do
      Hub.unsubscribe username
      onDisconnect

handleJoinGame :: forall m. (GameState m, MonadLobby m, MonadShuffle m) => Game.Id -> Username -> m ()
handleJoinGame gameId username = Lobby.withRoom gameId openRoom fullRoom
  where
    fullRoom :: Room 'Full -> m ()
    fullRoom room = do
      -- Hub.sendMessage
      pure ()

    openRoom :: Room 'Open -> m ()
    openRoom room = do
      Lobby.joinRoom username room >>= either (const pass) startGame

    startGame :: Room 'Full -> m ()
    startGame room = do
      shuffledDeck <- mkShuffledFullDeck
      Game.mk (room ^. #users) shuffledDeck |> either (const pass) GameState.addGame
