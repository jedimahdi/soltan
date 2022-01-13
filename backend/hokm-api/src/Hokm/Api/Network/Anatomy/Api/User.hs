module Hokm.Api.Network.Anatomy.Api.User
    ( ProfileResponse
    , Routes (..)
    ) where

import           Data.Generics.Labels             ()
import qualified Hokm.Api.Data.User               as User
import           Hokm.Api.Network.Anatomy.Prelude
import qualified Hokm.Api.Servant.Response        as Response
import qualified Hokm.Data.Validation             as Validation

type ProfileResponse = '[Response.Ok User.Readable, Response.Unauthorized, Response.NotFound]

data Routes route = Routes { profile :: route :- Auth :> Get '[JSON] ProfileResponse
                           }
  deriving stock (Generic)
