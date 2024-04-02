module Prelude (
    module Control.Lens,
    module Data.Map.Strict,
    module Data.UUID,
    module Flow,
    module Json,
    module Relude,
) where

import Control.Lens (at, ix, view, (%=), (%~), (.=), (.~), (^.), (^?))
import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Map.Strict (Map)
import Data.UUID (UUID)
import Flow (apply, (!>), (<!), (<|), (|>))
import Relude hiding (id, state)

-- type WithLog env m = Colog.WithLog env Logger.Message.Minimal m
