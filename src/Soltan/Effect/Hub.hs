module Soltan.Effect.Hub where

import Control.Exception.Safe (MonadMask, bracket)
import Control.Lens (at, sans)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Network.WebSockets as WS
import Soltan.App.Client (Client (..), Clients)
import qualified Soltan.App.Client as Client
import Soltan.App.Env (Has (..), grab)
import Soltan.App.Monad (App)
import Soltan.Data.Username (Username)

class Monad m => MonadHub m where
  type Connection m
  subscribe :: Username -> Connection m -> m ()
  unsubscribe :: Username -> m ()
  sendMessage :: BSL.ByteString -> Username -> m ()
  keepConnectionAlive :: Connection m -> m ()

instance MonadHub App where
  type Connection App = WS.Connection
  subscribe = subscribeImpl
  unsubscribe = unsubscribeImpl
  sendMessage = sendMessageImpl
  keepConnectionAlive = keepConnectionAliveImpl

sendJSON :: (ToJSON a, MonadHub m, Foldable t) => t a -> Username -> m ()
sendJSON jsonData username = traverse_ (\d -> sendMessage (Aeson.encode d) username) jsonData

sendJSON' :: (ToJSON a, MonadHub m) => a -> Username -> m ()
sendJSON' jsonData = sendJSON (Just jsonData)

type WithHub r m = (MonadReader r m, Has Clients r, MonadIO m, MonadMask m)

subscribeImpl :: WithHub r m => Username -> WS.Connection -> m ()
subscribeImpl username conn = do
  clientsVar <- grab
  atomically <| modifyTVar' clientsVar (Map.insert username (Client username conn))

unsubscribeImpl :: WithHub r m => Username -> m ()
unsubscribeImpl username = do
  clientsVar <- grab @Clients
  atomically <| modifyTVar' clientsVar (sans username)

withClient :: WithHub r m => Username -> (Client -> m ()) -> m ()
withClient username f = do
  clientsVar <- grab @Clients
  clientsMap <- readTVarIO clientsVar
  let client = clientsMap ^. at username
  maybe pass f client

sendMessageImpl :: WithHub r m => BSL.ByteString -> Username -> m ()
sendMessageImpl message username = do
  putStrLn $ "[Send Message " <> show username <> "]" <> show message
  withClient username \client -> liftIO <| WS.sendTextData (client ^. #connection) message

keepConnectionAliveImpl :: WithHub r m => WS.Connection -> m ()
keepConnectionAliveImpl conn = do
  _ <-
    liftIO . WS.withPingThread conn 30 pass
      <| liftIO . infinitely . WS.receiveData @ByteString
      <| conn
  pass
