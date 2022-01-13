module Hokm.Api.Effect.Hub
    where

import           Control.Lens         ( (^.) )
import qualified Data.Map             as Map
import           Hokm.Api.Data.Game   ( Game )
import qualified Hokm.Api.Data.Game   as Game
import           Network.WebSockets   ( Connection, sendTextData )
import           Polysemy             ( Embed, Members, Sem, interpret, makeSem )
import           Polysemy.AtomicState ( AtomicState, atomicGet, atomicModify' )

-- data HubType
--   = NotificationsHub
--   | GameHub Game.Id
--   deriving (Eq, Ord, Show)

type Hubs = Map Game.Id [Connection]

data HubL m a where
  Subscribe :: Game.Id -> Connection -> HubL m ()
  BroadcastMessage :: Game.Id -> ByteString -> HubL m ()

makeSem ''HubL

run :: Members [AtomicState Hubs, Embed IO] r => Sem (HubL ': r) a -> Sem r a
run = interpret \case
          Subscribe id conn -> do
            atomicModify' <| Map.insertWith (<>) id [conn]

          BroadcastMessage id message -> do
            hubs <- atomicGet
            let connections = Map.lookup id hubs
            forM_ connections (mapM_ (liftIO . (`sendTextData` message)))






