module Hokm.Api.Network.Server.Socket
    ( Effects
    , server
    ) where

import qualified Data.UUID                           as UUID
import           Hokm.Api.Data.User
import           Hokm.Api.Effect.GamesState          ( GamesStateL )
import           Hokm.Api.Effect.Hub                 ( HubL )
import           Hokm.Api.Effect.Random              ( RandomL )
import           Hokm.Api.Effect.WebSocket           ( WebSocketL )
import           Hokm.Api.Network.Anatomy.Socket
import qualified Hokm.Api.Network.Server.Socket.Game as Game
import qualified Hokm.Api.Servant.Response           as Response
import qualified Network.WebSockets                  as WS
import           Polysemy                            ( Embed, Member, Members, Sem, embed )
import           Polysemy.Error                      ( Error )
import           Servant                             ( Union, respond )
import           Servant.API.Generic
import           Servant.Server                      ( ServerError )
import           Servant.Server.Generic              ( AsServerT, genericServerT )

type Effects = '[HubL, RandomL, WebSocketL, GamesStateL]

server :: Members Effects r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { game = Game.server
                               }
