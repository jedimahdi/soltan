module Soltan.Game.Action.Modifiers
  ( Modifier
  , removeCardFromHand
  , addCardToMiddle
  , clearMiddle
  , nextTurn
  , userTurn
  , chooseHokm
  ) where

import qualified Soltan.Data.Modifier as Modifier
import Control.Lens (ix, (%~), (.~))
import Soltan.Data.Game
import Soltan.Data.Game.Card (Card, Suit)
import Soltan.Data.Username (Username)
import Data.List.PointedList.Circular (next, focus)

type Modifier = Modifier.Modifier Game

chooseHokm :: Suit -> Modifier
chooseHokm suit = Modifier.mk (#hokm .~ Choosed suit)

removeCardFromHand :: Username -> Card -> Modifier
removeCardFromHand username card
  = Modifier.mk (#hands . ix username %~ filter (/= card))

addCardToMiddle :: Username -> Card -> Modifier
addCardToMiddle username card
  = Modifier.mk (#middle %~ (:) (PlayedCard card username))

clearMiddle :: Modifier
clearMiddle = Modifier.mk (#middle .~ mempty)

nextTurn :: Modifier
nextTurn = Modifier.mk (#players %~ next)

userTurn :: Username -> Modifier
userTurn username = Modifier.mk (#players . focus .~ username)

