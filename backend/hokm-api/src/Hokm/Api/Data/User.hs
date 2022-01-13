{-# LANGUAGE UndecidableInstances #-}
module Hokm.Api.Data.User
    ( Creatable (..)
    , EmailAddress
    , Id
    , Password
    , Readable (..)
    , Table (..)
    , User
    , Username
    , parseRawCreatable
    ) where

import           Data.Aeson                            ( FromJSON, ToJSON )
import           Data.Generics.Labels                  ()
import           Database.PostgreSQL.Simple            ( FromRow, ToRow )
import qualified Hokm.Api.Data.Aeson.RecordErrorFormat as Aeson
import           Hokm.Api.Data.ByteString.Password     ( Password )
import qualified Hokm.Api.Data.ByteString.Password     as Password
import qualified Hokm.Api.Data.Id                      as Data
import           Hokm.Api.Data.Text.EmailAddress       ( EmailAddress )
import qualified Hokm.Api.Data.Text.EmailAddress       as EmailAddress
import           Hokm.Api.Data.Text.Username           ( Username )
import qualified Hokm.Api.Data.Text.Username           as Username
import           Hokm.Data.Validation                  ( Validate, Validation )
import qualified Hokm.Data.Validation                  as Validation
import qualified Hokm.Api.Effect.Scrypt as Scrypt

data User

type Id = Data.Id User

data Readable = Readable { id           :: Id
                         , username     :: Username
                         , emailAddress :: EmailAddress
                         }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, FromRow, ToJSON)

data Table = Table { id           :: Id
                   , username     :: Username
                   , emailAddress :: EmailAddress
                   , encryptedPassword :: Scrypt.Encrypted Password
                   }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow)

data Creatable p = Creatable { username     :: Validate p Text Username
                             , emailAddress :: Validate p Text EmailAddress
                             , password     :: Validate p Text Password
                             }
  deriving stock (Generic)

deriving stock instance Show (Creatable 'Validation.Raw)
deriving anyclass instance ToJSON   (Creatable 'Validation.Raw)
deriving anyclass instance FromJSON (Creatable 'Validation.Raw)

deriving anyclass instance ToRow (Creatable 'Validation.Parsed)

deriving stock instance Show (Creatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Creatable 'Validation.Error) instance ToJSON (Creatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Creatable 'Validation.Error) instance FromJSON (Creatable 'Validation.Error)

parseRawCreatable :: Creatable 'Validation.Raw -> Validation (Creatable 'Validation.Error) (Creatable 'Validation.Parsed)
parseRawCreatable = Validation.parseRecord Creatable { username = Username.mk, emailAddress = EmailAddress.parse, password = Password.mk }
