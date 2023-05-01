module Soltan.Network.Wai.Log (
  logMiddleware,
) where

import Colog (LogAction, usingLoggerT)
import Control.Lens ((^.))
import qualified Data.ByteString as W8
import qualified Data.List as List
import qualified Data.Text as Text
import Network.HTTP.Types (hAuthorization, hCookie)
import Network.HTTP.Types.Status (Status (..))
import Network.Wai (Middleware, Request (..), responseStatus)
import Network.Wai.Logger
import qualified Soltan.Logger as Logger
import qualified Soltan.Logger.Message as Logger.Message

apacheLog :: Request -> Int -> Text
apacheLog req responseCode =
  ip <> " - " <> "\"" <> decodeUtf8 method <> " " <> decodeUtf8 path <> " " <> show (httpVersion req) <> "\" " <> show responseCode
 where
  ip = toText . showSockAddr . remoteHost <| req
  path = rawPathInfo req <> rawQueryString req
  method = requestMethod req

logMiddleware :: LogAction IO Logger.Message.Minimal -> Middleware
logMiddleware logger app req respond =
  app req <| \response -> do
    let responseCode = statusCode . responseStatus <| response
    usingLoggerT logger <| Logger.info (apacheLog req responseCode)
    respond response
