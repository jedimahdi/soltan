{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Socket.Clients where

import Control.Concurrent.STM (swapTVar)
import Soltan.Data.Username (Username)
import Soltan.Effects.WebSocket (WebSocket, WebSocketMessaging, ConnectionId)
import qualified Soltan.Effects.WebSocket as WebSocket
import Soltan.Socket.Types (Client (..), Err (AuthFailed), ServerState (..), WithServerState)
import Control.Lens (over, at, (?~))
import Soltan.Data.Has (grab)

data LoginRequest = LoginRequest
  { username :: Username
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

authenticateClient :: WebSocketMessaging m => ConnectionId m -> m (Either Err Username)
authenticateClient conn = WebSocket.receiveJSON @LoginRequest conn <&> maybe (Left AuthFailed) (Right . view #username)
