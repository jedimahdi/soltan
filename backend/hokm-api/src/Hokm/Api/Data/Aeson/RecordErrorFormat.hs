{-# LANGUAGE UndecidableInstances #-}

module Hokm.Api.Data.Aeson.RecordErrorFormat
    ( RecordErrorFormat (..)
    ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import qualified Data.Aeson   as Aeson
import           GHC.Generics ( Rep )

newtype RecordErrorFormat (error :: Type)
  = RecordErrorFormat error

options :: Aeson.Options
options = Aeson.defaultOptions { Aeson.omitNothingFields = True }

instance (Generic error, Aeson.GToJSON Aeson.Zero (Rep error)) => ToJSON (RecordErrorFormat error) where
  toJSON (RecordErrorFormat e) = Aeson.genericToJSON options e

instance (Generic error, Aeson.GFromJSON Aeson.Zero (Rep error)) => FromJSON (RecordErrorFormat error) where
  parseJSON = fmap RecordErrorFormat . Aeson.genericParseJSON options
