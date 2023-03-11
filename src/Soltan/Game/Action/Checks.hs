module Soltan.Game.Action.Checks
  ( Checker
  , isHokmChoosen
  , isHokmNotChoosen
  , isRightTurn
  , isRightSuitToPlay
  , haveCard
  ) where

import Control.Lens (at, elemOf, folded, ix, non, traversed, view)
import Data.List.PointedList
import qualified Soltan.Data.Checker as Checker
import Soltan.Data.Game hiding (Error)
import Soltan.Data.Game.Action
import Soltan.Data.Game.Card
import Soltan.Data.Username (Username)

type Checker = Checker.Checker Game Error

isHokmChoosen :: Checker
isHokmChoosen = Checker.mk HokmNotChoosen ((/=) NotChoosed . view #hokm)

isHokmNotChoosen :: Checker
isHokmNotChoosen = Checker.invert HokmNotChoosen isHokmChoosen

isRightTurn :: Username -> Checker
isRightTurn username = Checker.mk WrongTurn ((==) username . view turnL)

haveCard :: Username -> Card -> Checker
haveCard username card = Checker.mk CardNotFound (elemOf (#hands . ix username . traverse) card)

isRightSuitToPlay :: Username -> Card -> Checker
isRightSuitToPlay username card = isBaseSuit <> isHokmSuit
  where
    cardSuit :: Suit
    cardSuit = card ^. #suit

    isBaseSuit :: Checker
    isBaseSuit = Checker.when (haveBaseSuit username) <| Checker.mk WrongSuit (maybe True (== cardSuit) . view baseSuitL)

    isHokmSuit :: Checker
    isHokmSuit = Checker.when (not . haveBaseSuit username) <| Checker.mk WrongSuit checkHokmSuit

    checkHokmSuit game
      = case game ^. #hokm of
          NotChoosed   -> False
          Choosed suit -> suit == cardSuit
