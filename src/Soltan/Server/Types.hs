module Soltan.Server.Types where

import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)

import Soltan.App.Monad (App)

type AppServer = AsServerT App
type ToApi (site :: Type -> Type) = ToServantApi site
