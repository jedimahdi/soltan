module Hokm.Api.Data.Jwt
    ( Jwt
    , JwtSections
    , PrivateClaims
    , decode
    , dissociate
    , getPrivateClaims
    , mk
    , reassociate
    ) where

import           Control.Exception      ( handle )
import           Control.Lens           ( _1, (%~) )
import qualified Data.ByteString        as W8
import qualified Data.ByteString.Char8  as C8
import           Data.Either.Validation ( validationToEither )
import qualified Data.List              as List
import           Data.Word8             ( _period )
import qualified Hokm.Api.Data.User     as User
import           Libjwt.Algorithms      ( Algorithm (ECDSA256) )
import           Libjwt.Keys            ( EcKeyPair (..) )
import           Libjwt.PrivateClaims   ( Ns (..), type (->>), withNs )
import qualified Libjwt.PrivateClaims   as Libjwt
import qualified Web.Libjwt             as Libjwt



algorithm :: Algorithm EcKeyPair
algorithm = ECDSA256 $ FromEcPem { ecPrivKey = private, ecPubKey = public }
 where
  private = C8.pack $ List.unlines
    [ "-----BEGIN EC PRIVATE KEY-----"
    , "MHQCAQEEILRL0jQ4pJt7hywTs8Fu2OXow+YBKVUy1mtROh8lNEPjoAcGBSuBBAAK"
    , "oUQDQgAEAocXIuapPURO9Zl79p9ATtYJpvk+62+WiFw9K3pHeOSgGkIWaGlyk7LB"
    , "DC4ldDxrQ9s6YsABIc2E0jIhKhCIOA=="
    , "-----END EC PRIVATE KEY-----"
    ]
  public = C8.pack $ List.unlines
    [ "-----BEGIN PUBLIC KEY-----"
    , "MFYwEAYHKoZIzj0CAQYFK4EEAAoDQgAEAocXIuapPURO9Zl79p9ATtYJpvk+62+W"
    , "iFw9K3pHeOSgGkIWaGlyk7LBDC4ldDxrQ9s6YsABIc2E0jIhKhCIOA=="
    , "-----END PUBLIC KEY-----"
    ]

type Namespace = "Hokm"

ns :: Ns Namespace
ns = Ns

type PrivateClaimsList = '["userId" ->> User.Id, "username" ->> User.Username]

data PrivateClaims = PrivateClaims { userId   :: User.Id
                                   , username :: User.Username
                                   }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Libjwt.FromPrivateClaims, Libjwt.ToPrivateClaims)

type Jwt = Libjwt.Jwt PrivateClaimsList ( 'Libjwt.SomeNs Namespace)

type HeaderAndPayload = ByteString
type Signature = ByteString
type JwtSections = (HeaderAndPayload, Signature)

mk :: User.Id -> User.Username -> JwtSections
mk userId username = Libjwt.sign algorithm payload |> Libjwt.getToken |> dissociate
 where
  payload = Libjwt.ClaimsSet { iss           = Libjwt.Iss Nothing
                             , sub           = Libjwt.Sub Nothing
                             , aud           = mempty
                             , exp           = Libjwt.Exp Nothing
                             , nbf           = Libjwt.Nbf Nothing
                             , iat           = Libjwt.Iat Nothing
                             , jti           = Libjwt.Jti Nothing
                             , privateClaims = ns `withNs` PrivateClaims { userId = userId, username = username }
                             }

decode :: ByteString -> IO (Maybe Jwt)
decode token =
  Libjwt.jwtFromByteString settings mempty algorithm token
    |> fmap (fmap Libjwt.getValid . rightToMaybe . validationToEither)
    |> handle @SomeException (const $ pure Nothing)
  where settings = Libjwt.Settings { leeway = 5, appName = Just "Hokm" }

getPrivateClaims :: Jwt -> PrivateClaims
getPrivateClaims = Libjwt.fromPrivateClaims . Libjwt.privateClaims . Libjwt.payload

separator :: Word8
separator = _period
{-# INLINE separator #-}

isSeparator :: Word8 -> Bool
isSeparator = (==) separator
{-# INLINE isSeparator #-}

dissociate :: ByteString -> JwtSections
dissociate jwt = W8.breakEnd isSeparator jwt |> _1 %~ W8.init
{-# INLINE dissociate #-}

reassociate :: ByteString -> ByteString -> ByteString
reassociate headerAndPayload sig = W8.intercalate (one separator) [headerAndPayload, sig]
{-# INLINE reassociate #-}
