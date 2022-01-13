module Hokm.Api.Data.Text.EmailAddress
    ( EmailAddress
    , Error (..)
    , mk
    , parse
    , pattern EmailAddress
    , un
    ) where

import           Control.Lens.TH                      ( makeWrapped )
import           Data.Aeson                           ( FromJSON, ToJSON )
import           Database.PostgreSQL.Simple.FromField ( FromField )
import           Database.PostgreSQL.Simple.ToField   ( ToField )
import qualified Hokm.Api.Data.Aeson.FieldErrorFormat as Aeson
import           Hokm.Data.Validation                 ( Validation )
import qualified Hokm.Data.Validation                 as Validation
import           Text.Email.Validate                  ( canonicalizeEmail )

newtype EmailAddress
  = Mk { un :: Text }
  deriving newtype (Eq, FromField, FromJSON, Show, ToField, ToJSON)

makeWrapped ''EmailAddress

pattern EmailAddress :: Text -> EmailAddress
pattern EmailAddress a <- Mk a
{-# COMPLETE EmailAddress #-}

data Error = IsInvalid deriving stock (Generic, Show)
  deriving (ToJSON) via Aeson.FieldErrorFormat Error
  deriving (FromJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

type instance Validation.Errors Text EmailAddress = Errors

mk :: Text -> Maybe EmailAddress
mk = fmap Mk . fmap decodeUtf8 . canonicalizeEmail . encodeUtf8

parse :: Text -> Validation Errors EmailAddress
parse = Validation.maybeToSuccess (IsInvalid :| []) . mk
