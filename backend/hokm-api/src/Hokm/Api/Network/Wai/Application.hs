module Hokm.Api.Network.Wai.Application
    ( application
    ) where

import           Hokm.Api.Network.Server
import           Hokm.Api.Servant.Authentication
import qualified Network.Wai                        as Wai
import           Polysemy                           ( Members, Sem )
import           Polysemy.Error                     ( Error )
import           Servant.Server                     ( Context (..), Handler, ServerError )
import           Servant.Server.Experimental.Auth
import           Servant.Server.Generic             ( genericServeTWithContext )

application :: Members (Error ServerError ': Effects) r => (forall a . Sem r a -> Handler a) -> Wai.Application
application nt = genericServeTWithContext nt server context where context = mkAuthHandler handleAuthentication :. EmptyContext
