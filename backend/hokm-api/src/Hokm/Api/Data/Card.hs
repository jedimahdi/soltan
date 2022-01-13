module Hokm.Api.Data.Card
    ( Card (..)
    , Deck
    , Suit (..)
    , Value (..)
    , makeDeck
    ) where

import           Data.Aeson           ( FromJSON, ToJSON )
import           Data.Generics.Labels ()

data Suit = Club | Diamond | Heart | Spade deriving stock (Enum, Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving stock
  ( Enum
  , Eq
  , Generic
  , Ord
  , Show
  )
  deriving anyclass (FromJSON, ToJSON)

data Card = Card { suit  :: Suit
                 , value :: Value
                 }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


type Deck = [Card]

makeDeck :: Deck
makeDeck = do
    suit <- [Club .. Spade]
    value <- [Two .. Ace]
    pure <| Card suit value
