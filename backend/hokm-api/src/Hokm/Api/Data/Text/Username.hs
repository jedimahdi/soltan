module Hokm.Api.Data.Text.Username
    ( Error (..)
    , Errors
    , Username (..)
    , mk
    , pattern Username
    , un
    ) where


import           Control.Lens.TH                      ( makeWrapped )
import           Control.Selective                    ( ifS )
import           Data.Aeson                           ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import qualified Data.Text                            as Text
import           Database.PostgreSQL.Simple.FromField ( FromField )
import           Database.PostgreSQL.Simple.ToField   ( ToField )
import qualified Hokm.Api.Data.Aeson.FieldErrorFormat as Aeson
import qualified Hokm.Data.Char                       as Char
import           Hokm.Data.Validation                 ( Validation )
import qualified Hokm.Data.Validation                 as Validation
import           Libjwt.Classes                       ( JwtRep )

newtype Username
  = Mk { un :: Text }
  deriving newtype
  ( Eq
  , FromField
  , FromJSON
  , FromJSONKey
  , JwtRep ByteString
  , Ord
  , Show
  , ToField
  , ToJSON
  , ToJSONKey
  )

makeWrapped ''Username

pattern Username :: Text -> Username
pattern Username a <- Mk a
{-# COMPLETE Username #-}

data Error = IsEmpty | IsShort | IsLong | IsInvalid deriving stock (Generic, Show)
  deriving (ToJSON) via Aeson.FieldErrorFormat Error
  deriving (FromJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

mk :: Text -> Validation Errors Username
mk input = Mk input <$ validate input

type instance Validation.Errors Text Username = Errors

validate :: Text -> Validation Errors ()
validate input = ifS
  (pure $ Text.null input)
  (Validation.failure IsEmpty)
  (  Validation.failureIf (l > 20) IsLong
  *> Validation.failureIf (l < 2) IsShort
  *> Validation.failureUnless (Text.all Char.isAlphaNumUnderscore input) IsInvalid
  )
  where l = Text.length input
