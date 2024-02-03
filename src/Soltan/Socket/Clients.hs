module Soltan.Socket.Clients where

import Control.Concurrent.STM (writeTChan)
import Control.Concurrent.STM.TChan (newTChan)
import Control.Lens (sans)
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import Soltan.Data.Username (Username)
import Soltan.Logger.Severity (Severity (..))
import Soltan.Socket.Types
import Soltan.Socket.Utils

checkAddClient :: Server -> MsgIn -> WS.Connection -> IO (Maybe Client)
checkAddClient server msg conn = do
  case msg of
    Login username -> do
      atomically $ do
        clientmap <- readTVar (server ^. #clients)
        if Map.member username clientmap
          then pure Nothing
          else do
            client <- newClient username conn
            writeTVar (server ^. #clients) <| Map.insert username client clientmap
            pure <| Just client
    _ -> pure Nothing

newClient :: Username -> WS.Connection -> STM Client
newClient username connection = do
  sendChan <- newTChan
  status <- newTVar Idle
  pure <| Client{..}

removeClient :: Server -> Username -> IO ()
removeClient server@Server{log} username = do
  log Debug $ "User " <> show username <> " has disconnected."
  atomically do
    modifyTVar' (server ^. #clients) (sans username)

sendMessage :: Client -> Message -> STM ()
sendMessage client = writeTChan (client ^. #sendChan)
