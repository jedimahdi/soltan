module Prelude
    ( module Colog
    , module Control.Lens
    , module Data.Map.Strict
    , module Data.UUID
    , module Flow
    , module Json
    , module Relude
    , WithLog
    ) where

import           Colog           (LogAction (..), Severity (..), log, pattern D,
                                  pattern E, pattern I, pattern W)
import qualified Colog           (Message, WithLog)
import           Control.Lens    (view, (%~), (.~), (^.))
import           Data.Aeson      as Json (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Map.Strict (Map)
import           Data.UUID       (UUID)
import           Flow            (apply, (!>), (<!), (<|), (|>))
import           Relude          hiding (id)

type WithLog env m = Colog.WithLog env Colog.Message m
