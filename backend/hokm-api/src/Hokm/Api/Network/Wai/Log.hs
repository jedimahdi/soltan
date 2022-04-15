module Hokm.Api.Network.Wai.Log
    ( logMiddleware
    ) where

import           Colog                           ( LogAction, usingLoggerT )
import           Control.Lens                    ( (^.) )
import qualified Data.ByteString                 as W8
import qualified Data.List                       as List
import qualified Data.Text                       as Text
import qualified Hokm.Api.Data.Jwt               as Jwt
import           Hokm.Api.Servant.Authentication
import qualified Hokm.Data.ByteString            as W8
import qualified Hokm.Logger                     as Logger
import qualified Hokm.Logger.Message             as Logger.Message
import           Network.HTTP.Types              ( hAuthorization, hCookie )
import           Network.HTTP.Types.Status       ( Status (..) )
import           Network.Wai                     ( Middleware, Request (..), responseStatus )
import           Network.Wai.Logger
import           Web.Cookie                      ( parseCookies )

getClaims :: Request -> IO (Maybe Jwt.PrivateClaims)
getClaims request = maybe (pure Nothing) ((fmap . fmap) Jwt.getPrivateClaims . Jwt.decode) token
 where
  cookies          = request |> requestHeaders |> List.lookup hCookie |> fmap parseCookies
  tokenFromCookies = Jwt.reassociate <$> (cookies >>= List.lookup headerAndPayloadCookieName) <*> (cookies >>= List.lookup signatureCookieName)
  tokenFromAuthorization =
    (request |> requestHeaders |> List.lookup hAuthorization) >>= fmap W8.trim . W8.stripPrefix authorizationHeaderPrefix
  token = tokenFromCookies <|> tokenFromAuthorization

apacheLog :: Request -> Int -> Maybe Jwt.PrivateClaims -> Text
apacheLog req responseCode mclaims =
  ip <> " - " <> userId <> "\"" <> decodeUtf8 method <> " " <> decodeUtf8 path <> " " <> show (httpVersion req) <> "\" " <> show responseCode
 where
  ip     = toText . showSockAddr . remoteHost <| req
  path   = rawPathInfo req <> rawQueryString req
  method = requestMethod req
  userId = case mclaims of
    Nothing     -> "- "
    Just claims -> show (claims ^. #userId) <> " "

logMiddleware :: LogAction IO Logger.Message.Minimal -> Middleware
logMiddleware logger app req respond = app req <| \response -> do
  let responseCode = statusCode . responseStatus <| response
  claims <- getClaims req
  usingLoggerT logger <| Logger.info (apacheLog req responseCode claims)
  respond response
