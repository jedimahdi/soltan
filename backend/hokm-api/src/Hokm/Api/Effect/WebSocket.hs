module Hokm.Api.Effect.WebSocket where

import qualified Data.Aeson         as Aeson
import qualified Network.WebSockets as WS
import           Polysemy           ( Embed, Member, Sem, embed, interpret, makeSem )


data WebSocketL m a where
  StayAlive :: WS.Connection -> WebSocketL m ()
  RecieveData :: Aeson.FromJSON a => WS.Connection -> WebSocketL m (Maybe a)

makeSem ''WebSocketL

run :: Member (Embed IO) r => Sem (WebSocketL ': r) a -> Sem r a
run = interpret \case
    StayAlive conn -> do
      _ <- liftIO . WS.withPingThread conn 30 pass <|
        liftIO . infinitely . WS.receiveData @ByteString <| conn
      pass

    RecieveData conn -> do
      liftIO . fmap Aeson.decodeStrict . WS.receiveData <| conn

