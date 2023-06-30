module Soltan.Socket where

import Colog (usingLoggerT)
import Data.Map as Map
import Pipes (Consumer, Pipe, Producer, Producer', await, for, runEffect, yield, (>->))
import qualified Pipes.Aeson
import Pipes.Concurrent (Input, Output, newest)
import qualified Pipes.Prelude as Pipes
import Soltan.Data.Username (Username)
import Soltan.Effects.Clients (ManageClients)
import qualified Soltan.Effects.Clients as Clients
import Soltan.Effects.Concurrent (Concurrent)
import qualified Soltan.Effects.Concurrent as Concurrent
import Soltan.Effects.Lobby (ManageLobby)
import qualified Soltan.Effects.Lobby as Lobby
import Soltan.Effects.LogMessages (HasLog, LogMessages)
import qualified Soltan.Effects.LogMessages as Logger
import Soltan.Effects.Now (Now)
import Soltan.Effects.Random (MonadRandom)
import Soltan.Effects.WebSocket (ConnectionId, WebSocket, WebSocketMessaging)
import qualified Soltan.Effects.WebSocket as WebSocket
import Soltan.Hokm (Game)
import Soltan.Logger.Message (Scope (..))
import qualified Soltan.Logger.Message as Logger.Message
import Soltan.Socket.Clients (authenticateClient)
import Soltan.Socket.Lobby (initialLobby)
import Soltan.Socket.Msg (msgHandler)
import Soltan.Socket.Table (setupTablePipeline)
import Soltan.Socket.Types (
  Client (..),
  Err (..),
  Lobby,
  MsgIn,
  MsgOut (..),
  ServerState (..),
  Table (..),
  TableDoesNotExistInLobby (..),
  TableName,
  Task (..),
 )
import Soltan.SocketApp (SocketApp, logScopedMessageToStdStreams, mkEnv, runSocketApp)

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState{clients = mempty, lobby = lobby}

runSocketServer :: Int -> IO ()
runSocketServer port = do
  lobby <- initialLobby
  serverStateTVar <- newTVarIO (initialServerState lobby)
  let logAction = Logger.Message.Scoped WebSocket >$< logScopedMessageToStdStreams
  let env = mkEnv serverStateTVar logAction
  runSocketApp env (run port)

run :: (WebSocket m, HasLog m, ManageLobby m, ManageClients m, Concurrent m, MonadRandom m) => Int -> m ()
run port = do
  Logger.info <| "Websocket server on ws://localhost:" <> show port
  lobby <- Lobby.getLobby
  traverse_
    (uncurry setupTablePipeline)
    (Map.toList lobby)
  WebSocket.runServer port \conn -> do
    app conn

app :: (WebSocketMessaging m, HasLog m, ManageLobby m, ManageClients m, Concurrent m, MonadRandom m) => ConnectionId m -> m ()
app conn = authenticateClient conn >>= either authenticateFailed (\username -> go username `Concurrent.finally` disconnect username)
 where
  go username = do
    WebSocket.sendJSON conn (AuthSuccess username)
    Logger.debug <| show username <> " Connected."
    (writeMsgInSource, readMsgInSource) <- Concurrent.spawn <| newest 1
    (writeMsgOutSource, readMsgOutSource) <- Concurrent.spawn <| newest 1

    let client = Client{outgoingMailbox = writeMsgOutSource, ..}
    Clients.addClient client

    Concurrent.forkProcess <| msgOutsWorker conn readMsgOutSource
    Concurrent.forkProcess <| msgInsWorker readMsgInSource writeMsgOutSource client

    void
      <| infinitely
      <| runEffect
      <| WebSocket.producer @MsgIn conn
      >-> Concurrent.toOutput writeMsgInSource

  authenticateFailed = WebSocket.sendJSON conn . ErrMsg

disconnect :: (HasLog m, ManageClients m) => Username -> m ()
disconnect username = do
  Logger.debug <| show username <> " Disconnected..."
  Clients.removeClient username

msgOutsWorker :: (Concurrent m, WebSocketMessaging m) => ConnectionId m -> Input MsgOut -> m ()
msgOutsWorker conn readMsgOutSource = void . infinitely . runEffect <| pipeline
 where
  pipeline = Concurrent.fromInput readMsgOutSource >-> WebSocket.consumer conn

msgInsWorker :: (Concurrent m, ManageLobby m, HasLog m, MonadRandom m) => Input MsgIn -> Output MsgOut -> Client -> m ()
msgInsWorker readMsgInSource writeMsgOutSource client = void . infinitely . runEffect <| pipeline
 where
  pipeline =
    Concurrent.fromInput readMsgInSource
      >-> Pipes.mapM (msgHandler (client ^. #username))
      >-> runCommands writeMsgOutSource
      >-> Concurrent.toOutput writeMsgOutSource

runCommands :: forall m. (Concurrent m, ManageLobby m) => Output MsgOut -> Pipe [Task] MsgOut m ()
runCommands writeMsgOutSource = await >>= traverse_ interpretTask
 where
  interpretTask :: Task -> Producer' MsgOut m ()
  interpretTask (SendMsg msgOut) = yield msgOut
  interpretTask (JoinLobby tableName username) = Lobby.addSubscriber tableName username
  interpretTask (NewGameState tableName game) =
    lift <| Lobby.withTable tableName pass \table ->
      runEffect <| yield game >-> Concurrent.toOutput (table ^. #gameInMailbox)
