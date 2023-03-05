module Soltan.Data.Game.Card
  ( Card(..)
  , Value(..)
  , Suit(..)
  ) where

import Data.Generics.Labels ()

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving stock
  ( Enum
  , Eq
  , Generic
  , Ord
  , Show
  )

data Suit = Club | Diamond | Heart | Spade
  deriving stock (Enum, Eq, Generic, Ord, Show)

data Card = Card { suit  :: Suit
                 , value :: Value
                 }
  deriving stock (Eq, Show, Generic, Ord)

