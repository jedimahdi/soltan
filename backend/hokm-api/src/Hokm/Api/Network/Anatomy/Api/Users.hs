module Hokm.Api.Network.Anatomy.Api.Users
    ( CreateResponse
    , GetAllResponse
    , Routes (..)
    ) where

import           Data.Generics.Labels             ()
import qualified Hokm.Api.Data.User               as User
import           Hokm.Api.Network.Anatomy.Prelude
import qualified Hokm.Api.Servant.Response        as Response
import qualified Hokm.Data.Validation             as Validation

type CreateResponse = '[Response.Created User.Readable , Response.Conflict , Response.Unprocessable (User.Creatable 'Validation.Error)]

type GetAllResponse = '[Response.Ok [User.Readable]]

data Routes route = Routes { create :: route :- ReqBody '[JSON] (User.Creatable 'Validation.Raw) :> Post '[JSON] CreateResponse
                           , getAll :: route :- Get '[JSON] GetAllResponse
                           }
  deriving stock (Generic)
