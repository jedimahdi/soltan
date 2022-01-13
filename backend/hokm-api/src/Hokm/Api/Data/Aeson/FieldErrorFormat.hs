{-# LANGUAGE UndecidableInstances #-}

module Hokm.Api.Data.Aeson.FieldErrorFormat
    ( FieldErrorFormat (..)
    ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import qualified Data.Aeson   as Aeson
import           GHC.Generics ( Rep )

newtype FieldErrorFormat (error :: Type)
  = FieldErrorFormat error

options :: Aeson.Options
options = Aeson.defaultOptions { Aeson.tagSingleConstructors = True, Aeson.omitNothingFields = True }

instance (Generic error, Aeson.GToJSON Aeson.Zero (Rep error)) => ToJSON (FieldErrorFormat error) where
  toJSON (FieldErrorFormat e) = Aeson.genericToJSON options e

instance (Generic error, Aeson.GFromJSON Aeson.Zero (Rep error)) => FromJSON (FieldErrorFormat error) where
  parseJSON = fmap FieldErrorFormat . Aeson.genericParseJSON options
