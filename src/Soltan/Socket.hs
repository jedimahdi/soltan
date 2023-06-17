{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Soltan.Socket where

import qualified Control.Concurrent.STM as STM
import Control.Lens (itraverse, itraversed, ix, (^..), (^?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Generics.Labels ()
import Data.Map as Map
import qualified Network.WebSockets as WS
import Pipes (Consumer, Pipe, Producer, Producer', await, for, runEffect, yield, (>->))
import qualified Pipes.Aeson
import Pipes.Concurrent (Input, Output, newest)
import Pipes.Parse (draw)
import qualified Pipes.Prelude as Pipes
import Soltan.Data.Has (Has, grab)
import Soltan.Data.Username (Username)
import Soltan.Effects.Clients (ManageClients)
import qualified Soltan.Effects.Clients as Clients
import Soltan.Effects.Concurrent (Concurrent)
import qualified Soltan.Effects.Concurrent as Concurrent
import Soltan.Effects.Lobby (ManageLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (LogMessages)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Now (Now)
import Soltan.Effects.Random (MonadRandom)
import Soltan.Effects.WebSocket (WebSocket)
import qualified Soltan.Effects.WebSocket as WebSocket
import Soltan.Hokm.Types (Game)
import Soltan.Socket.Clients (LoginRequest (..), addClient, authenticateClient)
import Soltan.Socket.Lobby (initialLobby)
import Soltan.Socket.Msg (msgHandler)
import Soltan.Socket.Prelude
import Soltan.Socket.Table (setupTablePipeline, withTable)
import Soltan.Socket.Types (
  Client (..),
  Command (..),
  Err (..),
  Lobby,
  MsgHandlerConfig (..),
  MsgIn,
  MsgOut (..),
  ServerState (..),
  Table (..),
  TableDoesNotExistInLobby (..),
  TableName,
  WithServerState,
 )
import Soltan.Socket.Utils (encodeMsgToJSON)
import Soltan.SocketApp (mkEnv, runSocketApp, runWithClient)
import UnliftIO (MonadUnliftIO, finally)
import UnliftIO.Async (Async, async)

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState{clients = mempty, lobby = lobby}

runRun :: Int -> IO ()
runRun port = do
  lobby <- initialLobby
  serverStateTVar <- newTVarIO (initialServerState lobby)
  -- let env = mkEnv serverState
  -- runSocketApp env app
  traverse_
    (uncurry $ setupTablePipeline serverStateTVar)
    (Map.toList lobby)

run :: WebSocket.WebSocketServer m => Int -> m ()
run port = do
  WebSocket.runServer port \conn -> do
    pass

runSocketServer :: Int -> IO ()
runSocketServer port = do
  lobby <- initialLobby

  serverStateTVar <- newTVarIO (initialServerState lobby)

  traverse_
    (uncurry $ setupTablePipeline serverStateTVar)
    (Map.toList lobby)

  _ <- WS.runServer "0.0.0.0" port <| go serverStateTVar
  pass
 where
  go :: TVar ServerState -> WS.PendingConnection -> IO ()
  go serverState pending = do
    conn <- WS.acceptRequest pending
    let env = mkEnv conn serverState
    WS.withPingThread conn 30 pass do
      runSocketApp env app

wreceiveJSON :: forall a m. (WebSocket m, FromJSON a, LogMessages m, Now m, Show a) => m (Maybe a)
wreceiveJSON = do
  msg <- WebSocket.receive
  Logger.debug <| "Recieved Msg " <> show msg
  let m = Aeson.decode . BS.fromStrict <| msg
  Logger.debug <| "Parsed Msg is " <> show m
  pure m

wproducer :: forall a m. (WebSocket m, FromJSON a, LogMessages m, Now m, Show a) => Producer a m ()
wproducer =
  void . infinitely <| do
    mMsg <- lift wreceiveJSON
    case mMsg of
      Nothing -> lift <| Logger.warning "Msg format is wrong"
      Just msg -> yield msg

app :: (WebSocket m, LogMessages m, Now m, ManageLobby m, ManageClients m, Concurrent m, MonadRandom m) => m ()
app = authenticateClient >>= either authenticateFailed (\username -> go username `Concurrent.finally` disconnect username)
 where
  go username = do
    WebSocket.sendJSON AuthSuccess
    Logger.debug <| show username <> " Connected."
    (writeMsgInSource, readMsgInSource) <- Concurrent.spawn <| newest 1
    (writeMsgOutSource, readMsgOutSource) <- Concurrent.spawn <| newest 1

    let client = Client{outgoingMailbox = writeMsgOutSource, ..}
    Clients.addClient client

    Concurrent.forkProcess <| msgOutsWorker readMsgOutSource
    Concurrent.forkProcess <| msgInsWorker readMsgInSource writeMsgOutSource client

    void
      <| infinitely
      <| runEffect
      <| wproducer @MsgIn
      >-> Concurrent.toOutput writeMsgInSource

  authenticateFailed = WebSocket.sendJSON . ErrMsg

disconnect :: Username -> AppL ()
disconnect username = do
  Logger.debug <| show username <> " Disconnected..."
  Clients.removeClient username

msgOutsWorker :: (Concurrent m, WebSocket m) => Input MsgOut -> m ()
msgOutsWorker readMsgOutSource = void . infinitely . runEffect <| pipeline
 where
  pipeline = Concurrent.fromInput readMsgOutSource >-> WebSocket.consumer

msgInsWorker :: Input MsgIn -> Output MsgOut -> Client -> AppL ()
msgInsWorker readMsgInSource writeMsgOutSource client = void . infinitely . runEffect <| pipeline
 where
  pipeline =
    Concurrent.fromInput readMsgInSource
      >-> Pipes.mapM (msgHandler client)
      >-> runCommands writeMsgOutSource
      >-> Concurrent.toOutput writeMsgOutSource

runCommands :: forall m. (Concurrent m, ManageLobby m) => Output MsgOut -> Pipe (Either Err [Command]) MsgOut m ()
runCommands writeMsgOutSource = await >>= either (yield . ErrMsg) (traverse_ interpretCommand)
 where
  interpretCommand :: Command -> Producer' MsgOut m ()
  interpretCommand (SendMsg msgOut) = yield msgOut
  interpretCommand (JoinLobby tableName username) = lift <| Lobby.addSubscriber tableName username
  interpretCommand (NewGameState tableName game) =
    lift <| Lobby.withTable tableName pass \table ->
      runEffect <| yield game >-> Concurrent.toOutput (table ^. #gameInMailbox)
