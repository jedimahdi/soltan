module Soltan.Server.Socket where

import Soltan.Data.Username
import Soltan.Data.Game (Game)
import qualified Soltan.Data.Game as Game
import Soltan.Effect.GameState
import Soltan.Effect.Hub (MonadHub)
import qualified Soltan.Effect.Hub as Hub
import qualified Network.WebSockets as WS
import Control.Exception.Safe (finally, MonadMask)

withConnection :: MonadHub WS.Connection m => MonadMask m => WS.Connection -> Username -> m () -> m ()
withConnection conn username app = flip finally handleDisconnect do
  Hub.subscribe conn username
  app
  Hub.stayAlive
  where
    handleDisconnect = Hub.unsubscribe username

handleJoinGame :: GameState m => Game.Id -> Username -> m ()
handleJoinGame gameId username = do
  -- findGameById gameId >>= maybe handleNewGame handleGame
    
  pure ()
