module Hokm.Api.Effect.Hub where

import           Control.Lens         ( view, (^.) )
import qualified Data.Aeson           as Aeson
import           Data.Generics.Labels ()
import qualified Data.Map             as Map
import           Hokm.Api.Data.Game   ( Game )
import qualified Hokm.Api.Data.Game   as Game
import qualified Hokm.Api.Data.User   as User
import           Network.WebSockets   ( Connection, sendTextData )
import           Polysemy             ( Embed, Members, Sem, interpret, makeSem )
import           Polysemy.AtomicState ( AtomicState, atomicGet, atomicModify' )


data ConnectionData = ConnectionData { sock     :: Connection
                                     , username :: User.Username
                                     }
  deriving stock (Generic)

type Hubs = Map Game.Id [ConnectionData]

data HubL m a where
  Subscribe :: Game.Id -> Connection -> User.Username -> HubL m ()
  BroadcastMessage :: Aeson.ToJSON a => Game.Id -> a -> HubL m ()
  BroadcastMessageWithUsername :: Aeson.ToJSON a => Game.Id -> (User.Username -> Maybe a) -> HubL m ()

makeSem ''HubL

run :: Members [AtomicState Hubs, Embed IO] r => Sem (HubL ': r) a -> Sem r a
run = interpret \case
          Subscribe id conn username -> do
            atomicModify' <| Map.insertWith (<>) id <| [ConnectionData conn username]

          BroadcastMessage id message -> do
            hubs <- atomicGet
            let connections = Map.lookup id hubs |> (fmap . fmap) (view #sock)
            let encodedMessage = message |> Aeson.encode |> toStrict
            forM_ connections (mapM_ (liftIO . (`sendTextData` encodedMessage)))

          BroadcastMessageWithUsername id withUsername -> do
            hubs <- atomicGet
            let mcs = Map.lookup id hubs
            case mcs of
              Just cs -> forM_ cs <| \c -> do
                let maybeMessage = withUsername (c ^. #username)
                case maybeMessage of
                  Just message ->
                    liftIO <| sendTextData (c ^. #sock) (message |> Aeson.encode |> toStrict)
                  Nothing -> pass
              Nothing -> pass
