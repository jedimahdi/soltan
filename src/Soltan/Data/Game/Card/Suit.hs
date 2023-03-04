module Soltan.Data.Game.Card.Suit (Suit(..)) where

data Suit = Club | Diamond | Heart | Spade
  deriving stock (Enum, Eq, Generic, Ord, Show)
