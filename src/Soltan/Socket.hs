{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Soltan.Socket where

import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Network.WebSockets as WS
import Pipes (Pipe, Producer, await, for, runEffect, yield, (>->))
import qualified Pipes.Aeson
import Pipes.Concurrent (Input, Output (send), fromInput, newest, spawn, toOutput)
import Pipes.Parse (draw)
import Soltan.App.Monad (App, AppEnv, runApp)
import Soltan.Data.Username (Username)
import Soltan.Socket.Msg (msgHandler)
import Soltan.Socket.Types (Client (..), Err (..), Lobby (..), MsgHandlerConfig (..), MsgIn, MsgOut (..), ServerState (..), Table (..), TableName, TableDoesNotExistInLobby (..))
import Soltan.Socket.Utils (encodeMsgToJSON, unLobby)
import Soltan.Data.Game (Game)

data LoginRequest = LoginRequest
  { username :: Username
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

authClient :: Monad m => BL.ByteString -> m (Either Err Username)
authClient loginData = do
  case Aeson.decode' @LoginRequest loginData of
    Nothing -> pure $ Left AuthFailed
    Just LoginRequest{..} -> pure $ Right username

sendMsg :: MonadIO m => WS.Connection -> MsgOut -> m ()
sendMsg conn msg = liftIO <| WS.sendTextData conn (encodeMsgToJSON msg)

initialServerState :: Lobby -> ServerState
initialServerState lobby = ServerState{clients = Map.empty, lobby = lobby}

initialLobby :: IO Lobby
initialLobby = do
  chan <- atomically STM.newBroadcastTChan
  -- randGen <- getStdGen
  -- let shuffledDeck' = shuffledDeck randGen
  (output, input) <- spawn $ newest 1
  let table =
        Table
          { subscribers = []
          , gameInMailbox = output
          , gameOutMailbox = input
          , -- , game = initialGameState shuffledDeck'
            channel = chan
          }
  return $ Lobby $ Map.fromList [("Black", table)]

-- setUpTablePipes ::
--   ConnectionString -> TVar ServerState -> TableName -> Table -> IO (Async ())
-- setUpTablePipes connStr s name Table {..} = do
--   t <- dbGetTableEntity connStr name
--   let (Entity key _) = fromMaybe notFoundErr t
--   async $
--     forever $
--       runEffect $
--         gamePipeline
--           connStr
--           s
--           key
--           name
--           gameOutMailbox
--           gameInMailbox

runSocketServer :: Int -> AppEnv -> IO ()
runSocketServer port env = do
  lobby <- initialLobby
  serverStateTVar <- newTVarIO (initialServerState lobby)
  _ <- async . WS.runServer "0.0.0.0" port <| \pending -> runApp env <| application serverStateTVar pending
  pass

websocketInMailbox :: MsgHandlerConfig -> IO (Output MsgIn, Output MsgOut)
websocketInMailbox conf@MsgHandlerConfig{..} = do
  (writeMsgInSource, readMsgInSource) <- spawn $ newest 1
  (writeMsgOutSource, readMsgOutSource) <- spawn $ newest 1
  void
    <| async
    <| forever
    <| runEffect
    <| fromInput readMsgInSource
    >-> msgInHandler conf
    >-> toOutput writeMsgOutSource -- process received MsgIn's and place resulting MsgOut in outgoing mailbox
  void . async <| socketMsgOutWriter clientConn readMsgOutSource -- send encoded MsgOuts from outgoing mailbox to socket
  return (writeMsgInSource, writeMsgOutSource)

msgInHandler :: MsgHandlerConfig -> Pipe MsgIn MsgOut IO ()
msgInHandler conf@MsgHandlerConfig{..} = do
  msgIn <- await
  res <- lift $ runReaderT (msgHandler msgIn) conf
  case res of
    Left err -> yield $ ErrMsg err
    -- Right (NewGameState tableName g) ->
    --   liftIO $ atomically $ updateGameState serverStateTVar tableName g
    Right m -> yield m

updateGameState :: TVar ServerState -> TableName -> Game -> STM ()
updateGameState serverStateTVar tableName newGame = do
  mbGameInMailbox' <- updateTableAndGetMailbox serverStateTVar tableName newGame
  case mbGameInMailbox' of
    Nothing -> return ()
    Just gameInMailbox' -> void (send gameInMailbox' newGame)

updateTableAndGetMailbox ::
  TVar ServerState -> TableName -> Game -> STM (Maybe (Output Game))
updateTableAndGetMailbox serverStateTVar tableName newGame = do
  ServerState {..} <- readTVar serverStateTVar
  case Map.lookup tableName $ unLobby lobby of
    Nothing -> throwSTM $ TableDoesNotExistInLobby tableName
    Just table@Table {..} -> do
      -- let updatedLobby = updateTableGame tableName newGame lobby
      -- swapTVar serverStateTVar ServerState {lobby = updatedLobby, ..}
      return $ Just gameInMailbox

socketMsgOutWriter :: WS.Connection -> Input MsgOut -> IO (Async ())
socketMsgOutWriter conn is =
  forever $
    runEffect $
      for
        (fromInput is >-> msgOutEncoder)
        (lift . WS.sendTextData conn)

application :: MonadIO m => TVar ServerState -> WS.PendingConnection -> m ()
application s pending = do
  conn <- liftIO <| WS.acceptRequest pending
  liftIO <| WS.forkPingThread conn 30
  liftIO $ putStrLn "Connected .."
  authMsg :: BL.ByteString <- liftIO <| WS.receiveData conn
  eUsername <- authClient authMsg
  case eUsername of
    Right username -> do
      (incomingMailbox, outgoingMailbox) <- liftIO . websocketInMailbox <| msgConf conn username
      let client = Client{..}
      sendMsg conn AuthSuccess
      forever $ do
        m :: BS.ByteString <- liftIO <| WS.receiveData conn
        runEffect $
          msgInDecoder (yield m >-> logMsgIn)
            >-> toOutput incomingMailbox
        pass
    Left err -> sendMsg conn (ErrMsg err)
 where
  msgConf c username =
    MsgHandlerConfig
      { serverStateTVar = s
      , clientConn = c
      , ..
      }

logMsgIn :: MonadIO m => Pipe BS.ByteString BS.ByteString m ()
logMsgIn = do
  msg <- await
  lift $ putStrLn "logging MsgIn"
  liftIO $ print msg
  yield msg

msgInDecoder :: MonadIO m => Producer BS.ByteString m () -> Producer MsgIn m ()
msgInDecoder rawMsgProducer = do
  (x, p') <- lift $ runStateT Pipes.Aeson.decode rawMsgProducer
  case x of
    Nothing -> return ()
    Just (Left a) -> do
      (y, p'') <- lift $ runStateT draw p'
      lift $ putStrLn "Left err"
      lift $ print y
      -- x is the problem input msg which failed to parse. We ignore it here by just resuming
      msgInDecoder p''
    Just c@(Right msgIn) -> do
      -- successful parsing case
      --   lift $ print c
      yield msgIn
      msgInDecoder p'

msgOutEncoder :: MonadIO m => Pipe MsgOut Text m ()
msgOutEncoder = do
  msgOut <- await
  lift $ putStrLn "Encoding msg: "
  lift $ print msgOut
  yield $ encodeMsgToJSON msgOut
