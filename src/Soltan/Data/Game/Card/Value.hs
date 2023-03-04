module Soltan.Data.Game.Card.Value (Value(..)) where

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving stock
  ( Enum
  , Eq
  , Generic
  , Ord
  , Show
  )
