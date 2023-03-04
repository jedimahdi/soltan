module Soltan.Data.Game.Card
  ( Card(..)
  , module Soltan.Data.Game.Card.Suit
  , module Soltan.Data.Game.Card.Value
  ) where

import Soltan.Data.Game.Card.Suit
import Soltan.Data.Game.Card.Value

data Card = Card { suit  :: Suit
                 , value :: Value
                 }
  deriving stock (Eq, Show, Generic, Ord)
