module Hokm.Data.Maybe
    ( module Data.Maybe
    , toSuccess
    ) where

import           Data.Maybe
import           Validation ( Validation )
import qualified Validation


toSuccess :: e -> Maybe a -> Validation (NonEmpty e) a
toSuccess e = Validation.maybeToSuccess (e :| [])
