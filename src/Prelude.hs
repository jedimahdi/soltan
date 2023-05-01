module Prelude
    ( module Control.Lens
    , module Data.Map.Strict
    , module Data.UUID
    , module Flow
    , module Json
    , module Relude
    -- , WithLog
    ) where

-- import qualified Colog           (WithLog)
import           Control.Lens    (view, (%~), (.~), (^.))
import           Data.Aeson      as Json (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Map.Strict (Map)
import           Data.UUID       (UUID)
import           Flow            (apply, (!>), (<!), (<|), (|>))
import           Relude          hiding (id)

-- type WithLog env m = Colog.WithLog env Logger.Message.Minimal m
