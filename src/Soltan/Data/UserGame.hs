{-# LANGUAGE NamedFieldPuns #-}

module Soltan.Data.UserGame where

import Control.Lens (at)
import Soltan.Data.Game (Game, PlayedCard, playersL, turnL)
import qualified Soltan.Data.Game as Game
import Soltan.Data.Game.Card (Card)
import Soltan.Data.Username (Username)

data UserGame = UserGame
  { id :: Game.Id
  , players :: [Username]
  , turn :: Username
  , cards :: [Card]
  , middle :: [PlayedCard]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

mkUserGame :: Username -> Game -> Maybe UserGame
mkUserGame username game = do
  cards <- game ^. #hands . at username
  pure
    <| UserGame
      { id = game ^. #id
      , players = game ^. playersL
      , turn = game ^. turnL
      , cards
      , middle = game ^. #middle
      }
