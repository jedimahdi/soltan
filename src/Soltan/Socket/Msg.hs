module Soltan.Socket.Msg where

import Soltan.Socket.Types (
  Err (..),
  MsgHandlerConfig (..),
  MsgIn (..),
  MsgOut (..),
  ServerState (ServerState, clients, lobby),
  Table (..),
  TableName,
 )
import Soltan.Socket.Utils (unLobby)

msgHandler :: MsgIn -> ReaderT MsgHandlerConfig IO (Either Err MsgOut)
msgHandler = undefined
-- msgHandler GetTables{} = getTablesHandler
-- msgHandler msg@SubscribeToTable{} = subscribeToTableHandler msg
