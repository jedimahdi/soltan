module Soltan.Data.Username
    ( Error (..)
    , Errors
    , Username (..)
    , mk
    , pattern Username
    , un
    ) where

import           Data.Aeson                           ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import qualified Data.Text                            as Text

newtype Username
  = Mk { un :: Text }
  deriving newtype
  ( Eq
  , Ord
  , Show
  )


pattern Username :: Text -> Username
pattern Username a <- Mk a
{-# COMPLETE Username #-}

data Error = IsEmpty | IsShort | IsLong | IsInvalid deriving stock (Generic, Show)
  -- deriving (ToJSON) via Aeson.FieldErrorFormat Error
  -- deriving (FromJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

mk :: Text -> Either Errors Username
mk input = Right $ Mk input

-- type instance Validation.Errors Text Username = Errors
--
-- validate :: Text -> Validation Errors ()
-- validate input = ifS
--   (pure $ Text.null input)
--   (Validation.failure IsEmpty)
--   (  Validation.failureIf (l > 20) IsLong
--   *> Validation.failureIf (l < 2) IsShort
--   *> Validation.failureUnless (Text.all Char.isAlphaNumUnderscore input) IsInvalid
--   )
--   where l = Text.length input
