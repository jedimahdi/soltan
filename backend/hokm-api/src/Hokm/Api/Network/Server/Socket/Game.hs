module Hokm.Api.Network.Server.Socket.Game
    ( server
    ) where

import           Control.Lens                         ( (^.) )
import qualified Data.Aeson                           as Aeson
import qualified Hokm.Api.Data.Authentication         as Authentication
import           Hokm.Api.Data.Card                   ( Card )
import           Hokm.Api.Data.Game                   ( Game )
import qualified Hokm.Api.Data.Game                   as Game
import           Hokm.Api.Data.Session                ( Session )
import qualified Hokm.Api.Data.Session                as Session
import qualified Hokm.Api.Data.User                   as User
import qualified Hokm.Api.Effect.Database.User        as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User        as Database.User
import           Hokm.Api.Effect.GameState            ( GameStateL )
import qualified Hokm.Api.Effect.GameState            as GameState
import           Hokm.Api.Effect.Hub                  ( HubL )
import qualified Hokm.Api.Effect.Hub                  as Hub
import           Hokm.Api.Effect.Random               ( RandomL )
import qualified Hokm.Api.Effect.Random               as Random
import           Hokm.Api.Network.Anatomy.Socket.Game
import qualified Hokm.Api.Servant.Response            as Response
import qualified Hokm.Data.Validation                 as Validation
import qualified Network.WebSockets                   as WS
import           Polysemy                             ( Embed, Member, Members, Sem )
import           Servant                              ( Union, respond )
import           Servant.API.Generic                  ( ToServant )
import           Servant.Server.Generic
import           Validation                           ( validation )

handleJoinGame :: Members '[Embed IO, GameStateL, HubL, RandomL] r => Game.Id -> WS.Connection -> Sem r ()
handleJoinGame gameId conn = do
  maybeUser :: Maybe Authentication.User <- WS.receiveData conn |> fmap Aeson.decodeStrict |> liftIO
  case maybeUser of
    Nothing -> pass
    Just user -> do
      deck <- Random.makeSuffledDeck
      maybeGame <- GameState.modifyGame gameId <| Game.joinedGame deck (user ^. #username)
      case maybeGame of
        Nothing -> pass
        Just game -> do
          Hub.subscribe gameId conn
          Hub.broadcastMessage gameId (toStrict <| Aeson.encode game)

          liftIO <| infinitely <| WS.receiveData @ByteString conn
          pass

server :: Members '[Embed IO, GameStateL, HubL, RandomL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { joinGame = handleJoinGame }
