module Hokm.Api.Network.Server.Api
    ( Effects
    , server
    ) where

import qualified Data.UUID                                  as UUID
import qualified Database.PostgreSQL.Simple                 as Database
import           Hokm.Api.Data.User
import qualified Hokm.Api.Effect.Database.User              as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User              as Database.User
import           Hokm.Api.Effect.GameState                  ( GameStateL )
import           Hokm.Api.Effect.Hub                        ( HubL )
import           Hokm.Api.Effect.Random                     ( RandomL )
import           Hokm.Api.Effect.Scrypt                     ( ScryptL )
import           Hokm.Api.Network.Anatomy.Api
import qualified Hokm.Api.Network.Server.Api.Authentication as Authentication
import qualified Hokm.Api.Network.Server.Api.Game           as Game
import qualified Hokm.Api.Network.Server.Api.User           as User
import qualified Hokm.Api.Network.Server.Api.Users          as Users
import qualified Hokm.Api.Servant.Response                  as Response
import           Polysemy                                   ( Embed, Members, Sem )
import           Polysemy.Error                             ( Error )
import qualified Polysemy.Reader                            as Polysemy ( Reader )
import           Servant.API.Generic
import           Servant.Server                             ( ServerError )
import           Servant.Server.Generic                     ( AsServerT, genericServerT )

type Effects = '[GameStateL, HubL, Embed IO, ScryptL, Database.UserL, Error ServerError, RandomL]

server :: Members Effects r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { authentication = Authentication.server
                               , users          = Users.server
                               , user           = User.server
                               , game           = Game.server
                               }
