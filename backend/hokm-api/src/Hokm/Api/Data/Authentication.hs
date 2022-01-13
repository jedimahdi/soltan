{-# LANGUAGE UndecidableInstances #-}
module Hokm.Api.Data.Authentication
    ( Credential (..)
    , User (..)
    , parseRawCredential
    ) where

import           Data.Aeson                            ( FromJSON, ToJSON )
import qualified Hokm.Api.Data.Aeson.RecordErrorFormat as Aeson
import qualified Hokm.Api.Data.ByteString.Password     as Password
import qualified Hokm.Api.Data.Text.Username           as Username
import qualified Hokm.Api.Data.User                    as User
import           Hokm.Data.Validation                  ( Validate, Validation )
import qualified Hokm.Data.Validation                  as Validation

data Credential (p :: Validation.Phase) = Credential { username :: Validate p Text User.Username
                                                     , password :: Validate p Text User.Password
                                                     }
  deriving stock (Generic)

deriving stock instance Show (Credential 'Validation.Raw)
deriving anyclass instance ToJSON (Credential 'Validation.Raw)
deriving anyclass instance FromJSON (Credential 'Validation.Raw)

deriving via Aeson.RecordErrorFormat (Credential 'Validation.Error) instance ToJSON (Credential 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Credential 'Validation.Error) instance FromJSON (Credential 'Validation.Error)

parseRawCredential :: Credential 'Validation.Raw -> Validation (Credential 'Validation.Error) (Credential 'Validation.Parsed)
parseRawCredential = Validation.parseRecord Credential { username = Username.mk, password = Password.mk }


data User = User { username :: User.Username
                 , token    :: Text
                 }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

