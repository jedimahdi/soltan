module Hokm.Api.Network.Anatomy.Api
    ( Routes (..)
    ) where

import           Data.Generics.Labels                        ()
import qualified Hokm.Api.Network.Anatomy.Api.Authentication as Authentication
import qualified Hokm.Api.Network.Anatomy.Api.Game           as Game
import qualified Hokm.Api.Network.Anatomy.Api.User           as User
import qualified Hokm.Api.Network.Anatomy.Api.Users          as Users
import           Servant
import           Servant.API.Generic

data Routes route = Routes { authentication :: route :- "authentication" :> ToServantApi Authentication.Routes
                           , users :: route :- "users" :> ToServantApi Users.Routes
                           , user :: route :- "user" :> ToServantApi User.Routes
                           , game :: route :- "game" :> ToServantApi Game.Routes
                           }
  deriving stock (Generic)
