module Hokm.Api.Network.Server.Api.User
    ( server
    ) where

import           Control.Lens                      ( (^.) )
import           Hokm.Api.Data.Session             ( Session )
import qualified Hokm.Api.Data.Session             as Session
import qualified Hokm.Api.Data.User                as User
import qualified Hokm.Api.Effect.Database.User     as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User     as Database.User
import           Hokm.Api.Effect.Scrypt            ( ScryptL )
import           Hokm.Api.Network.Anatomy.Api.User
import qualified Hokm.Api.Servant.Response         as Response
import qualified Hokm.Data.Validation              as Validation
import           Polysemy                          ( Member, Members, Sem )
import           Servant                           ( Union, respond )
import           Servant.API.Generic               ( ToServant )
import           Servant.Server.Generic
import           Validation                        ( validation )

handleProfile :: Members '[Database.UserL] r => Session -> Sem r (Union ProfileResponse)
handleProfile = Session.withAuthenticated \jwt -> do
  muser <- Database.User.findById (jwt ^. #userId)
  case muser of
    Nothing   -> respond <| Response.notFound
    Just user -> respond <| Response.Ok <| user

server :: Members '[Database.UserL , ScryptL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { profile = handleProfile }
