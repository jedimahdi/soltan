module Hokm.Api.Network.Server
    ( Effects
    , server
    ) where

import qualified Database.PostgreSQL.Simple     as Database
import           Hokm.Api.Effect.Database.User  ( UserL )
import           Hokm.Api.Effect.GameState      ( GameStateL )
import           Hokm.Api.Effect.Hub            ( HubL )
import           Hokm.Api.Effect.Lobby          ( LobbyL )
import           Hokm.Api.Effect.Random         ( RandomL )
import           Hokm.Api.Effect.Scrypt         ( ScryptL )
import           Hokm.Api.Network.Anatomy
import qualified Hokm.Api.Network.Server.Api    as Api
import qualified Hokm.Api.Network.Server.Socket as Socket
import           Polysemy                       ( Embed, Member, Members, Sem, embed )
import           Polysemy.Error                 ( Error )
import qualified Polysemy.Reader                as Polysemy ( Reader )
import           Servant.Server                 ( ServerError )
import           Servant.Server.Generic         ( AsServerT )

type Effects = '[Embed IO, GameStateL, HubL, UserL, ScryptL, Error ServerError, RandomL, LobbyL]

server :: Members Effects r => Routes (AsServerT (Sem r))
server = Routes { api = Api.server, socket = Socket.server }
