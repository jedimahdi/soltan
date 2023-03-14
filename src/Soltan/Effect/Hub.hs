{-# LANGUAGE FunctionalDependencies #-}
module Soltan.Effect.Hub where

import           Control.Lens         (at, sans)
import qualified Data.Map             as Map
import qualified Network.WebSockets   as WS
import           Soltan.App.Client    (Client (..), Clients)
import           Soltan.App.Env       (Has (..), grab)
import           Soltan.App.Monad     (App)
import           Soltan.Data.Username (Username)

class Monad m => MonadHub c m | m -> c where
  subscribe   :: Username -> c -> m ()
  unsubscribe :: Username -> m ()
  sendMessage :: ByteString -> Username -> m ()
  keepConnectionAlive :: c -> m ()

instance MonadHub WS.Connection App where
  subscribe = subscribeImpl
  unsubscribe = unsubscribeImpl
  sendMessage = sendMessageImpl
  keepConnectionAlive = keepConnectionAliveImpl

type WithHub r m = (MonadReader r m, Has Clients r, MonadIO m)

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

sendMessageImpl :: WithHub r m => ByteString -> Username -> m ()
sendMessageImpl message username = do
  withClient username \client -> liftIO <| WS.sendTextData (client ^. #connection) message

keepConnectionAliveImpl :: WithHub r m => WS.Connection -> m ()
keepConnectionAliveImpl conn = do
  _ <- liftIO . WS.withPingThread conn 30 pass <|
    liftIO . infinitely . WS.receiveData @ByteString <| conn
  pass
