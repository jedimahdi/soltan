module Soltan.Socket.Clients where

import Soltan.Data.Username (Username)
import Soltan.Effects.WebSocket (ConnectionId, WebSocket, WebSocketMessaging)
import qualified Soltan.Effects.WebSocket as WebSocket
import Soltan.Socket.Types (Err (AuthFailed), MsgIn (..))
import Soltan.Effects.LogMessages (HasLog)

authenticateClient :: forall m. (WebSocketMessaging m, HasLog m) => ConnectionId m -> m (Either Err Username)
authenticateClient conn = go 3
 where
  go :: Int -> m (Either Err Username)
  go 0 = pure <| Left AuthFailed
  go n = do
    msg <- WebSocket.receiveJSON @MsgIn conn
    case msg of
      Just (Login username) -> pure <| Right username
      _ -> go (n - 1)
