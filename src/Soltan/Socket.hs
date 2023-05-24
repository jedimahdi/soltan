{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Socket where

import qualified Control.Concurrent.STM as STM
import Control.Lens (ix, (^?))
import qualified Data.Aeson as Aeson
import Data.Generics.Labels ()
import qualified Network.WebSockets as WS
import Pipes (Pipe, Producer, await, for, runEffect, yield, (>->))
import qualified Pipes.Aeson
import Pipes.Concurrent (Input, Output (send), fromInput, newest, spawn, toOutput)
import Pipes.Parse (draw)
import Soltan.Data.Has (Has, grab)
import Soltan.Data.Username (Username)
import Soltan.Effects.WebSocket (WebSocket)
import qualified Soltan.Effects.WebSocket as WebSocket
import Soltan.Hokm.Types (Game)
import Soltan.Logger (WithLog)
import qualified Soltan.Logger as Logger
import Soltan.Socket.Clients (LoginRequest (..), addClient, authenticateClient)
import Soltan.Socket.Lobby (initialLobby)
import Soltan.Socket.Msg (msgHandler)
import Soltan.Socket.Table (withTable)
import Soltan.Socket.Types (
  Client (..),
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
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (Async, async)

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState{clients = mempty, lobby = lobby}

runSocketServer :: Int -> IO ()
runSocketServer port = do
  lobby <- initialLobby
  serverStateTVar <- newTVarIO (initialServerState lobby)
  _ <- async . WS.runServer "0.0.0.0" port <| go serverStateTVar
  pass
 where
  go :: TVar ServerState -> WS.PendingConnection -> IO ()
  go serverState pending = do
    conn <- WS.acceptRequest pending
    let env = mkEnv conn serverState
    WS.withPingThread conn 30 pass do
      runSocketApp env app

app :: (WebSocket m, WithServerState env m, MonadUnliftIO m, WithLog env m) => m ()
app = authenticateClient >>= either authenticateFailed go
 where
  go username = do
    WebSocket.sendJSON AuthSuccess
    Logger.debug <| show username <> " Connected."
    (writeMsgInSource, readMsgInSource) <- liftIO <| spawn <| newest 1
    (writeMsgOutSource, readMsgOutSource) <- liftIO <| spawn <| newest 1

    let client = Client{outgoingMailbox = writeMsgOutSource, ..}
    addClient client

    forkHandleMsgOuts readMsgOutSource
    forkHandleMsgIns readMsgInSource writeMsgOutSource client

    void
      <| infinitely
      <| runEffect
      <| WebSocket.producer @MsgIn
      >-> toOutput writeMsgInSource

  authenticateFailed = WebSocket.sendJSON . ErrMsg

forkHandleMsgOuts :: (MonadUnliftIO m, WebSocket m) => Input MsgOut -> m ()
forkHandleMsgOuts readMsgOutSource = void <| async <| infinitely <| runEffect <| pipeline
 where
  pipeline = fromInput readMsgOutSource >-> WebSocket.consumer

forkHandleMsgIns :: (MonadUnliftIO m, WithServerState env m) => Input MsgIn -> Output MsgOut -> Client -> m ()
forkHandleMsgIns readMsgInSource writeMsgOutSource client = void <| async <| infinitely <| runEffect <| pipeline
 where
  pipeline = fromInput readMsgInSource >-> msgHandlerPipe client >-> handleNewGameStatePipe >-> toOutput writeMsgOutSource

msgHandlerPipe :: WithServerState env m => Client -> Pipe MsgIn MsgOut m ()
msgHandlerPipe client = await >>= msgHandler client >>= either (yield . ErrMsg) yield

handleNewGameStatePipe :: WithServerState env m => Pipe MsgOut MsgOut m ()
handleNewGameStatePipe = do
  msgOut <- await
  case msgOut of
    NewGameState tableName game -> do
      withTable tableName pass \table ->
        runEffect <| yield game >-> toOutput (table ^. #gameInMailbox)
    _ -> yield msgOut
