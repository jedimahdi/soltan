module Soltan.Hokm.ActionValidation where

import Control.Lens (elemOf, lengthOf, traversed)
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import Soltan.Hokm.Types
import Soltan.Hokm.Utils
import Unsafe.Coerce (unsafeCoerce)

validateAction :: Game -> Action -> Either GameErr ()
validateAction game action =
  case action of
    PlayCard idx card ->
      isInTrick game
        >> isRightTurn game idx
        >> checkHaveCard game idx card
        >> isRightSuitToPlay game idx card
    ChooseHokm idx _ -> isChoosingHokmPhase game >> isHakemChoosing game idx

-- canStartGame :: Game -> [Card] -> Four.Four Username -> Either GameErr ()
-- canStartGame GameBeforeStart _ _ = Right ()
-- canStartGame _ _ _ = Left GameAlreadyStarted
--
-- canNextTrick :: Game -> Either GameErr ()
-- canNextTrick (GameEndOfTrick _) = Right ()
-- canNextTrick _ = Left NotEndOfTrick

isChoosingHokmPhase :: Game -> Either GameErr ()
isChoosingHokmPhase (GameChoosingHokm _) = Right ()
isChoosingHokmPhase _ = Left NoChoosingHokmPhase

isHakemChoosing :: Game -> PlayerIndex -> Either GameErr ()
isHakemChoosing (GameChoosingHokm g) idx
  | g ^. #hakem == idx = Right ()
  | otherwise = Left NotHakem
isHakemChoosing _ _ = Left InvalidAction

isRightTurn :: Game -> PlayerIndex -> Either GameErr ()
isRightTurn (GameInProgress g) idx
  | g ^. #turn == idx = Right ()
  | otherwise = Left <| OutOfTurn (g ^. #turn)
isRightTurn _ _ = Left NoPlayerCanPlay

isInTrick :: Game -> Either GameErr ()
isInTrick (GameInProgress g) = Right ()
isInTrick (GameEndOfTrick _) = Left EndOfTrick
isInTrick (GameEnd _) = Left GameHasEnded
isInTrick _ = Left GameNotStarted

isRightSuitToPlay :: Game -> PlayerIndex -> Card -> Either GameErr ()
isRightSuitToPlay (GameInProgress g) idx card = case getBaseSuit g of
  Nothing -> Right ()
  Just baseSuit ->
    if haveSuit g idx baseSuit
      then if baseSuit == card ^. #suit then Right () else Left WrongSuit
      else Right ()
isRightSuitToPlay _ _ _ = Left InvalidAction

checkHaveCard :: Game -> PlayerIndex -> Card -> Either GameErr ()
checkHaveCard game idx card = bool (Left CardNotFound) (Right ()) (haveCard game idx card)
