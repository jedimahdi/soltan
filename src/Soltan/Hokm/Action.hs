module Soltan.Hokm.Action where

import Control.Lens (anyOf, elemOf, lengthOf, traversed)
import Data.List.Split (chunksOf)
import Soltan.Data.AtMostThree (AtMostThree (..))
import Soltan.Data.Four (Four (..))
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import Soltan.Hokm.ActionValidation (validateAction)
import Soltan.Hokm.Types hiding (Three, Two)
import Soltan.Hokm.Utils
import Prelude hiding (first, second)

runAction :: Action -> Game -> Either GameErr Game
runAction action game = do
  validateAction game action
  game
    |> case action of
      ChooseHokm idx suit -> chooseHokm idx suit
      PlayCard idx card -> addPlayerCardToBoard idx card . removeCardFromHand idx card . nextTurn
    |> pure

chooseHokm :: PlayerIndex -> Suit -> Game -> Game
chooseHokm idx suit (GameChoosingHokm g) = GameInProgress initialInProgress
 where
  initialInProgress :: GameInProgressState
  initialInProgress =
    GameInProgressState
      { turn = g ^. #hakem
      , hakem = g ^. #hakem
      , trumpSuit = suit
      , players = newPlayers
      , board = Zero
      , teamAPoints = 0
      , teamBPoints = 0
      , teamATricks = 0
      , teamBTricks = 0
      }
  (player1Cards, player2Cards, player3Cards, hakemCards') = g ^. #remainingDeck |> splitThreeWayWithRem 5
  hakemCards = hakemCards' <> g ^. #players . playerL (g ^. #hakem) . #cards
  u1 = g ^. #players . #player1 . #playerName
  u2 = g ^. #players . #player2 . #playerName
  u3 = g ^. #players . #player3 . #playerName
  u4 = g ^. #players . #player4 . #playerName
  newPlayers = case g ^. #hakem of
    Player1 ->
      mkPlayers (u1, hakemCards) (u2, player1Cards) (u3, player2Cards) (u4, player3Cards)
    Player2 ->
      mkPlayers (u1, player1Cards) (u2, hakemCards) (u3, player2Cards) (u4, player3Cards)
    Player3 ->
      mkPlayers (u1, player1Cards) (u2, player2Cards) (u3, hakemCards) (u4, player3Cards)
    Player4 ->
      mkPlayers (u1, player1Cards) (u2, player2Cards) (u3, player3Cards) (u4, hakemCards)
chooseHokm _ _ game = game

removeCardFromHand :: PlayerIndex -> Card -> Game -> Game
removeCardFromHand idx card = _GameInProgress . #players . playerL idx . #cards %~ filter (/= card)

addPlayerCardToBoard :: PlayerIndex -> Card -> Game -> Game
addPlayerCardToBoard idx card (GameInProgress g) = case g ^. #board of
  Zero -> g |> #board .~ One newCard |> GameInProgress
  One first -> g |> #board .~ Two first newCard |> GameInProgress
  Two first second -> g |> #board .~ Three first second newCard |> GameInProgress
  Three first second third ->
    let GameInProgressState{..} = g
     in GameEndOfTrick <| GameEndOfTrickState{board = Four.Four first second third newCard, ..}
 where
  newCard = PlayedCard card idx
addPlayerCardToBoard idx card g = g

nextTurn :: Game -> Game
nextTurn = _GameInProgress . #turn %~ nextPlayerIndexTurn
