module Hokm.Api.Network.Server.Socket.Game
    ( server
    ) where

import           Control.Lens
    ( at, index, itraversed, ix, sans, traverseOf, traversed, view, (.~), (?~), (^.) )
import           Control.Monad
import qualified Data.Aeson                           as Aeson
import qualified Hokm.Api.Data.Authentication         as Authentication
import           Hokm.Api.Data.Card                   ( Card )
import           Hokm.Api.Data.Game                   ( Game )
import qualified Hokm.Api.Data.Game                   as Game
import qualified Hokm.Api.Data.GameResponse           as GameResponse
import           Hokm.Api.Data.Session                ( Session )
import qualified Hokm.Api.Data.Session                as Session
import qualified Hokm.Api.Data.User                   as User
import qualified Hokm.Api.Effect.Database.User        as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User        as Database.User
import           Hokm.Api.Effect.GamesState           ( GamesStateL )
import qualified Hokm.Api.Effect.GamesState           as GamesState
import           Hokm.Api.Effect.Hub                  ( HubL )
import qualified Hokm.Api.Effect.Hub                  as Hub
import           Hokm.Api.Effect.Random               ( RandomL )
import qualified Hokm.Api.Effect.Random               as Random
import           Hokm.Api.Effect.WebSocket            ( WebSocketL )
import qualified Hokm.Api.Effect.WebSocket            as WebSocket
import           Hokm.Api.Network.Anatomy.Socket.Game
import qualified Hokm.Api.Servant.Response            as Response
import qualified Hokm.Data.Validation                 as Validation
import qualified Network.WebSockets                   as WS
import           Polysemy                             ( Embed, Member, Members, Sem )
import           Servant                              ( Union, respond )
import           Servant.API.Generic                  ( ToServant )
import           Servant.Server.Generic
import           Validation                           ( validation )

handleJoinGame :: Members '[HubL, RandomL, GamesStateL, WebSocketL] r => Game.Id -> WS.Connection -> Sem r ()
handleJoinGame gameId conn = whenJustM (WebSocket.recieveData conn) go
  where
    go :: Members '[HubL, RandomL, GamesStateL, WebSocketL] r => Authentication.User -> Sem r ()
    go user = do
      deck <- Random.makeSuffledDeck
      GamesState.joinGame gameId (Game.joinGame deck (user ^. #username)) >>= maybe pass \e -> do
        Hub.subscribe gameId conn (user ^. #username)
        either (Hub.broadcastMessage gameId . GameResponse.mkNotFullResponse) (Hub.broadcastMessageWithUsername gameId . GameResponse.mk) e
        WebSocket.stayAlive conn

server :: Members '[HubL, RandomL, GamesStateL, WebSocketL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { joinGame = handleJoinGame }
