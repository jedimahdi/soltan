module Hokm.Api.Network.Wai.Cors
    ( corsMiddleware
    ) where

import           Network.Wai                 ( Middleware, Request (..), responseStatus )
import           Network.Wai.Middleware.Cors

corsMiddleware :: Middleware
corsMiddleware = cors <| const <| Just policy
  where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["Content-Type", "Authorization"], corsMethods = "PUT" : simpleMethods }
