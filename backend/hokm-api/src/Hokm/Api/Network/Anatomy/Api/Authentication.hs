module Hokm.Api.Network.Anatomy.Api.Authentication
    ( LoginResponse
    , Routes (..)
    ) where

import           Data.Generics.Labels             ()
import qualified Hokm.Api.Data.Authentication     as Authentication
import           Hokm.Api.Network.Anatomy.Prelude
import qualified Hokm.Api.Servant.Response        as Response
import qualified Hokm.Data.Validation             as Validation
import           Web.Cookie                       ( SetCookie )

-- type LoginResponse = Headers '[Header "Set-Cookie" SetCookie , Header "Set-Cookie" SetCookie] NoContent

type LoginResponse = '[Response.Ok Authentication.User]

data Routes route = Routes { login :: route :- "login" :> ReqBody '[JSON] (Authentication.Credential 'Validation.Raw) :> Post '[JSON] LoginResponse
                           }
  deriving stock (Generic)
