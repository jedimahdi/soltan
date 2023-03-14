module Soltan.Server.Prelude
    ( module Servant.API
    , module Servant.API.Generic
    , module Soltan.Server.Types
    ) where

import           Servant.API         (Capture, Get, Header, Header', JSON,
                                      NoContent (NoContent), Post, QueryParam,
                                      QueryParam', ReqBody, (:>))
import           Servant.API.Generic (toServant, (:-))
import           Soltan.Server.Types

