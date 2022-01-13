module Hokm.Api.Data.Session
    ( Session (..)
    , withAnonymous
    , withAuthenticated
    ) where

import qualified Hokm.Api.Data.Jwt         as Jwt
import qualified Hokm.Api.Servant.Response as Response
import           Servant                   ( IsMember, Union, respond )

data Session
  = Authenticated Jwt.PrivateClaims
  | Anonymous
  deriving stock (Eq, Generic, Show)

withAnonymous :: Applicative m => IsMember Response.Forbidden as => m (Union as) -> Session -> m (Union as)
withAnonymous m Anonymous         = m
withAnonymous _ (Authenticated _) = respond Response.Forbidden

withAuthenticated :: Applicative m => IsMember Response.Unauthorized as => (Jwt.PrivateClaims -> m (Union as)) -> Session -> m (Union as)
withAuthenticated m (Authenticated x) = m x
withAuthenticated _ Anonymous         = respond Response.Unauthorized
