{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Socket.Clients where

import Control.Concurrent.STM (swapTVar)
import Soltan.Data.Username (Username)
import Soltan.Effects.WebSocket (WebSocket)
import qualified Soltan.Effects.WebSocket as WebSocket
import Soltan.Socket.Types (Client (..), Err (AuthFailed), ServerState (..), WithServerState)
import Control.Lens (over, at, (?~))
import Soltan.Data.Has (grab)

data LoginRequest = LoginRequest
  { username :: Username
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

authenticateClient :: WebSocket m => m (Either Err Username)
authenticateClient = WebSocket.receiveJSON @LoginRequest >>= pure . maybe (Left AuthFailed) (Right . view #username)

insertClient :: Client -> Map Username Client -> Map Username Client
insertClient client = at (client ^. #username) ?~ client

addClientSTM :: TVar ServerState -> Client -> STM ()
addClientSTM serverState client = do
  modifyTVar' serverState (over #clients (insertClient client))

addClient :: (WithServerState env m) => Client -> m ()
addClient client = do
  serverStateTVar <- grab @(TVar ServerState)
  atomically <| addClientSTM serverStateTVar client
