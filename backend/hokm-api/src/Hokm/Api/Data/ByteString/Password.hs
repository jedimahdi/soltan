module Hokm.Api.Data.ByteString.Password
    ( Error (..)
    , Errors
    , Password
    , mk
    , pattern Password
    , un
    ) where

import           Control.Selective                    ( ifS )
import           Data.Aeson                           ( FromJSON, ToJSON )
import qualified Data.Text                            as Text
import           Database.PostgreSQL.Simple.FromField ( FromField )
import           Database.PostgreSQL.Simple.ToField   ( ToField )
import qualified Hokm.Api.Data.Aeson.FieldErrorFormat as Aeson
import           Hokm.Data.Validation                 ( Validation )
import qualified Hokm.Data.Validation                 as Validation

newtype Password
  = Mk { un :: ByteString }
  deriving newtype (Eq, FromField, Show, ToField)

pattern Password :: ByteString -> Password
pattern Password a <- Mk a
{-# COMPLETE Password #-}

data Error = IsEmpty | IsShort deriving stock (Generic, Show)
  deriving (ToJSON) via Aeson.FieldErrorFormat Error
  deriving (FromJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

type instance Validation.Errors Text Password = Errors

mk :: Text -> Validation Errors Password
mk input = Mk (encodeUtf8 input) <$ validate input

validate :: Text -> Validation Errors ()
validate input = ifS (pure $ Text.null input) (Validation.failure IsEmpty) (Validation.failureIf (Text.length input < 3) IsShort)
