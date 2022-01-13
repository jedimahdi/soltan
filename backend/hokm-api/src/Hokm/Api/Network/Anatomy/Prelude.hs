module Hokm.Api.Network.Anatomy.Prelude
    ( Delete
    , Get
    , module Hokm.Api.Servant.Authentication
    , Patch
    , Post
    , module Servant.API
    , module Servant.API.Generic
    ) where

import           Hokm.Api.Servant.Authentication ( Auth )
import           Servant.API                     hiding ( Delete, Get, Patch, Post )
import           Servant.API.Generic

type Get = UVerb 'GET

type Post = UVerb 'POST

type Patch = UVerb 'PATCH

type Delete = UVerb 'DELETE
