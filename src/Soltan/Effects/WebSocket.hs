module Soltan.Effects.WebSocket where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WS
import Pipes (Consumer, Producer, await, yield)
import Soltan.Data.Has (Has, grab)
import Soltan.Socket.Types (Client)
import Soltan.SocketApp (SocketApp)
import UnliftIO (MonadUnliftIO, withRunInIO)

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
  msgIn :: ConnectionId m -> m ByteString
  msgOut :: ConnectionId m -> ByteString -> m ()

class Monad m => WebSocket m where
  receive :: m ByteString
  send :: ByteString -> m ()

instance WebSocket SocketApp where
  receive = receiveImpl
  send = sendImpl

type WithWebSocket r m = (MonadIO m, MonadReader r m, Has WS.Connection r)

receiveImpl :: WithWebSocket r m => m ByteString
receiveImpl = do
  conn <- grab @WS.Connection
  liftIO <| WS.receiveData conn

sendImpl :: WithWebSocket r m => ByteString -> m ()
sendImpl msg = do
  conn <- grab @WS.Connection
  liftIO <| WS.sendTextData conn msg

sendJSON :: forall a m. (WebSocket m, ToJSON a) => a -> m ()
sendJSON msg = send (BS.toStrict . Aeson.encode <| msg)

receiveJSON :: forall a m. (WebSocket m, FromJSON a) => m (Maybe a)
receiveJSON = Aeson.decode . BS.fromStrict <$> receive

producer :: forall a m. (WebSocket m, FromJSON a) => Producer a m ()
producer = void . infinitely <| whenJustM (lift receiveJSON) yield

consumer :: forall a m. (WebSocket m, ToJSON a) => Consumer a m ()
consumer = void . infinitely <| await >>= lift . sendJSON
