module Hokm.Api.Network.Server.Api.Authentication
    ( server
    ) where

import           Control.Lens                                ( _Wrapped', to, (^.) )
import qualified Data.Aeson                                  as Aeson
import           Hokm.Api.Data.Authentication                ( parseRawCredential )
import qualified Hokm.Api.Data.Authentication                as Authentication
import qualified Hokm.Api.Data.Id                            as Id
import qualified Hokm.Api.Data.Jwt                           as Jwt
import qualified Hokm.Api.Effect.Database.User               as Database ( UserL )
import qualified Hokm.Api.Effect.Database.User               as Database.User
import qualified Hokm.Api.Effect.Scrypt                      as Scrypt
import           Hokm.Api.Network.Anatomy.Api.Authentication
import           Hokm.Api.Servant.Authentication
    ( headerAndPayloadCookieName, signatureCookieName )
import qualified Hokm.Api.Servant.Response                   as Response
import qualified Hokm.Data.Validation                        as Validation
import           Polysemy                                    ( Members, Sem )
import           Polysemy.Error                              ( Error, throw )
import           Servant
import           Servant                                     ( Union, respond )
import           Servant.API.Generic
import           Servant.Server.Generic                      ( AsServerT, genericServerT )
import           Validation                                  ( validation )
import           Web.Cookie
import qualified Web.Cookie                                  as Cookie

handleLogin :: forall r . Members '[Database.UserL , Error ServerError] r => Authentication.Credential 'Validation.Raw -> Sem r (Union LoginResponse)
handleLogin = validation onValidationFailure onValidationSuccess . Authentication.parseRawCredential
  where
   onValidationFailure :: Authentication.Credential 'Validation.Error -> Sem r a
   onValidationFailure (Aeson.encode -> body) = throw err422 { errBody = body }

   onValidationSuccess :: Authentication.Credential 'Validation.Parsed -> Sem r (Union LoginResponse)
   onValidationSuccess credential = Database.User.findByUsername (credential ^. #username) >>= maybe
     (throw err401)
     \user -> do
       let doesPasswordsMatch = Scrypt.verifyPassword (credential ^. #password) (user ^. #encryptedPassword)
       unless doesPasswordsMatch $ throw err401
       let (headerAndPayload, sig) = Jwt.mk (user ^. #id) (user ^. #username)
       respond <| Response.Ok <| Authentication.User (credential ^. #username) (decodeUtf8 <| Jwt.reassociate headerAndPayload sig)


server :: Members '[Database.UserL , Error ServerError] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { login = handleLogin }

-- mkLoginResponse :: (ByteString, ByteString) -> LoginResponse
-- mkLoginResponse (headerAndPayload, sig) = NoContent |> addHeader setHeaderAndPayloadCookie |> addHeader setSignatureCookie
--  where
--   setHeaderAndPayloadCookie = mkSetHeaderAndPayloadCookie headerAndPayload
--   setSignatureCookie        = mkSetSignatureCookie sig
--
-- mkSetHeaderAndPayloadCookie :: ByteString -> SetCookie
-- mkSetHeaderAndPayloadCookie value = defaultSetCookie { setCookieName     = headerAndPayloadCookieName
--                                                      , setCookieValue    = value
--                                                      , setCookieSecure   = True
--                                                      , setCookieSameSite = Just Cookie.sameSiteStrict
--                                                      }
--
--
-- mkSetSignatureCookie :: ByteString -> SetCookie
-- mkSetSignatureCookie value = defaultSetCookie { setCookieName     = signatureCookieName
--                                               , setCookieValue    = value
--                                               , setCookieHttpOnly = True
--                                               , setCookieSecure   = True
--                                               , setCookieSameSite = Just Cookie.sameSiteStrict
--                                               }
