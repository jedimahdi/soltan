module Soltan.Socket where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (newTChan)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Exception
import Control.Lens (elemOf, folded)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Username as Username
import Soltan.Game.Types
import Soltan.Hokm (Game)
import qualified Soltan.Hokm as Game
import Soltan.Logger.Severity (Severity (..))
import Soltan.Socket.Clients
import Soltan.Socket.Types
import Soltan.Socket.Utils

import Soltan.App
import Soltan.App.Channel (readChannel, writeChannel)
import Soltan.App.Types

runSocketServer ::
  Int ->
  TVar (Map Username (Channel Outgoing)) ->
  Channel Command ->
  (Severity -> Text -> IO ()) ->
  IO ()
runSocketServer port outgoingChannels commandChannel log = do
  clients <- newTVarIO mempty
  let server = Server{..}
  log Info $ "Running server on port " <> show port
  WS.runServer "0.0.0.0" port \pending -> do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 pass do
      app' conn server

app' :: WS.Connection -> Server -> IO ()
app' conn server@Server{log} = do
  log Debug "New Connection"
  msg <- receiveLogin
  mask \restore -> do
    ok <- checkAddClient server msg conn
    case ok of
      Nothing ->
        restore <| do
          pass
      Just client -> do
        restore (runClient server client) `finally` disconnect server client
 where
  receiveLogin = do
    receiveJSON @MsgIn conn >>= \case
      Nothing -> throwIO (AssertionFailed "Parsing Login message failed")
      Just msg -> pure msg

runClient :: Server -> Client -> IO ()
runClient server@Server{log, outgoingChannels, commandChannel} client@Client{username} = do
  -- tablesMap <- readTVarIO (server ^. #tables)
  -- let isUserInTableAlready = find (elemOf (#users . folded) username) tablesMap
  atomically $ sendMessage client (Send (AuthSuccess username))
  -- case isUserInTableAlready of
  --   Nothing -> pass
  --   Just table -> do
  --     atomically $ sendMessage client (Command (JoinTable (table ^. #id)))
  channel <- newChannel
  atomically $ modifyTVar' outgoingChannels (Map.insert username channel)
  log Debug $ "User " <> show username <> " joined."
  Async.race_ (send channel) receive
 where
  receive = infinitely do
    rawMsg <- WS.receiveData (client ^. #connection)
    case Aeson.decode (BS.fromStrict rawMsg) of
      Nothing ->
        log Warning $ "[User] Parsing user msg failed: " <> show rawMsg
      Just msg -> do
        let command = case msg of
              NewTable -> AuthCommand (Logout username)
              _ -> AuthCommand (Logout username)
        writeChannel commandChannel command

  send channel = do
    msg <- readChannel channel
    WS.sendTextData (client ^. #connection) (BS.toStrict . Aeson.encode $ msg)

-- continue <- handleMessage server client msg
-- when continue serverThread

disconnect :: Server -> Client -> IO ()
disconnect server@Server{log, outgoingChannels} client@Client{username} = do
  removeClient server username
  atomically $ modifyTVar' outgoingChannels (Map.delete username)

-- clientStatus <- readTVarIO (client ^. #status)
-- case clientStatus of
--   InGame tableId -> do
--     leaveTable client server tableId
--   _ -> pass

-- handleMessage :: Server -> Client -> Message -> IO Bool
-- handleMessage server@Server{sendTableCommand, tables, log} client@Client{username} msg = do
--   log Debug $ "[User] " <> Username.un username <> " <- " <> show msg
--   case msg of
--     Notice txt ->
--       send $ Noti txt
--     Send m -> send m
--     GameInfo tableId game -> do
--       case Game.getPlayerIndexWithUsername username game of
--         Nothing -> pass
--         Just idx -> do
--           let summary = Game.mkGameSummary idx game
--           send $ NewGameStateSummary tableId summary
--     Command GetTables -> do
--       ts <- readTVarIO tables
--       send $ TableList (mkTableInfo <$> Map.elems ts)
--     Command (JoinTable id) -> do
--       joinTable client server id
--     Command (LeaveTable id) -> do
--       leaveTable client server id
--     Command NewTable -> do
--       sendTableCommand (NewGame username)
--       send $ Noti "Game being created..."
--     -- atomically $ modifyTVar' (server ^. #tables) (ix id . #users %~ ((client ^. #username) :))
--     Command (GameMsgIn (ChooseHokmMsg tableId suit)) -> do
--       tablesMap <- readTVarIO tables
--       case tablesMap ^? ix tableId of
--         Nothing -> pass
--         Just table -> do
--           atomically $ writeTChan (table ^. #chan) (ChooseHokm username suit)
--     Command (GameMsgIn (PlayCardMsg tableId card)) -> do
--       tablesMap <- readTVarIO tables
--       case tablesMap ^? ix tableId of
--         Nothing -> pass
--         Just table -> do
--           atomically $ writeTChan (table ^. #chan) (PlayCard username card)
--     _ -> pass
--   pure True
--  where
--   send :: (ToJSON a) => a -> IO ()
--   send msgToSend = WS.sendTextData (client ^. #connection) (BS.toStrict . Aeson.encode $ msgToSend)
--
-- joinTable :: Client -> Server -> TableId -> IO ()
-- joinTable client@Client{username} server@Server{tables, sendGameCommand} tableId = do
--   clientStatus <- readTVarIO (client ^. #status)
--   case clientStatus of
--     InGame clientGameTableId -> do
--       send $ Noti "You are already in game"
--       readTVarIO tables
--         >>= maybe pass (send . SuccessfullySubscribedToTable . mkTableInfo)
--           . Map.lookup clientGameTableId
--     Idle -> do
--       atomically $ do
--         tablesMap <- readTVar tables
--         case tablesMap ^? ix tableId of
--           Nothing -> pass
--           Just table -> do
--             case table ^. #status of
--               Open -> do
--                 let tablesUsers = table ^. #users
--                 let newTableUsers = username : tablesUsers
--                 case Four.mkFromList newTableUsers of
--                   Just four -> do
--                     modifyTVar' tables (ix tableId . #status .~ Started)
--                     writeTChan (table ^. #chan) (StartGame four)
--                   Nothing -> pass
--                 modifyTVar' tables (ix tableId . #users %~ (username :))
--                 writeTVar (client ^. #status) (InGame tableId)
--                 sendMessage client $ Send (SuccessfullySubscribedToTable (mkTableInfo table))
--                 clientsMap <- readTVar (server ^. #clients)
--                 -- Notify others
--                 forM_ tablesUsers \u -> do
--                   whenJust (clientsMap ^. at u) (\cl -> sendMessage cl $ Send (UpdateTable (mkTableInfo table)))
--               Started -> do
--                 let tableUsers = table ^. #users
--                 if username `elem` tableUsers
--                   then do
--                     modifyTVar' tables (ix tableId . #disconnectedUsers %~ filter (/= username))
--                     sendMessage client $ Send (SuccessfullySubscribedToTable (mkTableInfo table))
--                   else sendMessage client $ Send (Noti "Game already started")
--  where
--   send :: (ToJSON a) => a -> IO ()
--   send msgToSend = WS.sendTextData (client ^. #connection) (BS.toStrict . Aeson.encode $ msgToSend)
--
-- leaveTable :: Client -> Server -> TableId -> IO ()
-- leaveTable client@Client{username} server@Server{tables} tableId = do
--   tablesMap <- readTVarIO tables
--   case Map.lookup tableId tablesMap of
--     Nothing -> pass
--     Just table -> do
--       let tableStatus = table ^. #status
--       case tableStatus of
--         Open -> do
--           atomically $ modifyTVar' tables (ix tableId . #users %~ filter (/= username))
--         Started -> do
--           atomically $ modifyTVar' tables (ix tableId . #disconnectedUsers %~ (username :))
