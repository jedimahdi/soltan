module Soltan.Effects.WebSocket where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WS
import Pipes (Consumer, Producer, await, yield)
import Soltan.Data.Has (Has, grab)
import Soltan.Socket.Types (Client)
import Soltan.SocketApp (SocketApp)
import UnliftIO (MonadUnliftIO, withRunInIO)
import Soltan.Effects.LogMessages (HasLog)
import qualified Soltan.Effects.LogMessages as Logger

class Monad m => WebSocketServer m where
  type ConnectionId m :: Type
  runServer :: Int -> (ConnectionId m -> m ()) -> m ()

instance WebSocketServer SocketApp where
  type ConnectionId SocketApp = WS.Connection
  runServer = runServerImpl

runServerImpl :: MonadUnliftIO m => Int -> (WS.Connection -> m ()) -> m ()
runServerImpl port app =
  withRunInIO \runInIO ->
    WS.runServer "0.0.0.0" port \pending -> do
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 pass do
        runInIO <| app conn

class Monad m => WebSocketMessaging m where
  receive :: ConnectionId m -> m ByteString
  send :: ConnectionId m -> ByteString -> m ()

instance WebSocketMessaging SocketApp where
  receive = receiveImpl
  send = sendImpl

type WebSocket m = (WebSocketServer m, WebSocketMessaging m)

receiveImpl :: MonadIO m => WS.Connection -> m ByteString
receiveImpl conn = do
  d <- liftIO <| WS.receiveData conn
  print d
  pure d

sendImpl :: MonadIO m => WS.Connection -> ByteString -> m ()
sendImpl conn msg = liftIO <| WS.sendTextData conn msg

sendJSON :: forall a m. (WebSocketMessaging m, ToJSON a) => ConnectionId m -> a -> m ()
sendJSON conn msg = send conn (BS.toStrict . Aeson.encode <| msg)

receiveJSON :: forall a m. (WebSocketMessaging m, FromJSON a, HasLog m) => ConnectionId m -> m (Maybe a)
receiveJSON conn = do
  m <- receive conn
  Logger.debug <| "Received -> " <> show m
  pure <| Aeson.decode . BS.fromStrict <| m

producer :: forall a m. (WebSocketMessaging m, FromJSON a, HasLog m) => ConnectionId m -> Producer a m ()
producer conn = void . infinitely <| whenJustM (lift (receiveJSON conn)) yield

consumer :: forall a m. (WebSocketMessaging m, ToJSON a) => ConnectionId m -> Consumer a m ()
consumer conn = void . infinitely <| await >>= lift . sendJSON conn
