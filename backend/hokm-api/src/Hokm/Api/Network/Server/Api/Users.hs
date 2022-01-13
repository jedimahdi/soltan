module Hokm.Api.Network.Server.Api.Users
    ( server
    ) where

import           Control.Lens                       ( (^.) )
import qualified Hokm.Api.Data.User                 as User
import qualified Hokm.Api.Effect.Database.User      as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User      as Database.User
import           Hokm.Api.Effect.Scrypt             ( ScryptL )
import           Hokm.Api.Network.Anatomy.Api.Users
import qualified Hokm.Api.Servant.Response          as Response
import qualified Hokm.Data.Validation               as Validation
import           Polysemy                           ( Member, Members, Sem )
import           Servant                            ( Union, respond )
import           Servant.API.Generic                ( ToServant )
import           Servant.Server.Generic
import           Validation                         ( validation )

handleCreate :: Members '[Database.UserL , ScryptL] r => User.Creatable 'Validation.Raw -> Sem r (Union CreateResponse)
handleCreate = validation (respond . Response.Unprocessable) go . User.parseRawCreatable
 where
  go :: Members '[Database.UserL , ScryptL] r => User.Creatable 'Validation.Parsed -> Sem r (Union CreateResponse)
  go creatable = do
    -- hasConflict <- Database.User.doesExistsByUsernameOrEmailAddress (creatable ^. #username) (creatable ^. #emailAddress)
    -- if hasConflict then respond Response.Conflict else (respond . Response.Created) =<< Database.User.create creatable
    maybeUser <- Database.User.create creatable
    case maybeUser of
      Nothing   -> respond Response.Conflict
      Just user -> respond <| Response.Created <| user

handleGetAll :: Members '[Database.UserL] r => Sem r (Union GetAllResponse)
handleGetAll = do
  users <- Database.User.getAll
  respond <| Response.Ok <| users

server :: Members '[Database.UserL , ScryptL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { create = handleCreate, getAll = handleGetAll }
