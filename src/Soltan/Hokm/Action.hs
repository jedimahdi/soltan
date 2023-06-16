module Soltan.Hokm.Action where

import Control.Lens (anyOf, elemOf, lengthOf, traversed)
import Data.List ((!!))
import Data.List.Split (chunksOf)
import Soltan.Data.AtMostThree (AtMostThree (..))
import Soltan.Data.Four (Four (..))
import Soltan.Data.Username (Username)
import Soltan.Hokm.ActionValidation (validateAction)
import Soltan.Hokm.Types (
  Action (..),
  Card,
  ChoosingHokmState (..),
  Game (..),
  GameEndOfRoundState (
    GameEndOfRoundState,
    board,
    hakem,
    players,
    teamAPoints,
    teamARounds,
    teamBPoints,
    teamBRounds,
    trumpSuit
  ),
  GameErr,
  GameInProgressState (..),
  PlayedCard (PlayedCard),
  Player (..),
  PlayerIndex (..),
  Players (Players, player1, player2, player3, player4),
  Suit,
  Team (..),
  playerL,
  _GameInProgress,
 )
import Soltan.Hokm.Utils
import Prelude hiding (first, second)

runAction :: Action -> Game -> Either GameErr Game
runAction action game = do
  validateAction game action
  game
    |> case action of
      ChooseHokm idx suit -> chooseHokm idx suit
      PlayCard idx card -> addPlayerCardToBoard idx card . removeCardFromHand idx card . nextTurn
      NextRound -> nextRound
      StartGame deck usernames -> startGame deck usernames
    |> pure

startGame :: [Card] -> [Username] -> Game -> Game
startGame deck usernames GameBeforeStart =
  GameChoosingHokm
    <| ChoosingHokmState
      { hakem = Player1
      , remainingDeck = drop 5 deck
      , players =
          initialPlayers (usernames !! 0) (usernames !! 1) (usernames !! 2) (usernames !! 3)
            |> #player1 . #cards
            .~ take 5 deck
      }
startGame _ _ g = g

nextRound :: Game -> Game
nextRound (GameEndOfRound g@(GameEndOfRoundState{..})) =
  GameInProgress
    <| GameInProgressState{board = Zero, turn = findWinnerOfRound g, ..}
nextRound g = g

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
      , teamARounds = 0
      , teamBRounds = 0
      }
  (player1Cards, player2Cards, player3Cards, hakemCards') = g ^. #remainingDeck |> splitThreeWayWithRem 5
  hakemCards = hakemCards' <> g ^. #players . playerL (g ^. #hakem) . #cards
  newPlayers = case g ^. #hakem of
    Player1 ->
      Players
        { player1 = (g ^. #players . #player1){cards = hakemCards}
        , player2 = (g ^. #players . #player2){cards = player1Cards}
        , player3 = (g ^. #players . #player3){cards = player2Cards}
        , player4 = (g ^. #players . #player4){cards = player3Cards}
        }
    Player2 ->
      Players
        { player1 = (g ^. #players . #player1){cards = player1Cards}
        , player2 = (g ^. #players . #player2){cards = hakemCards}
        , player3 = (g ^. #players . #player3){cards = player2Cards}
        , player4 = (g ^. #players . #player4){cards = player3Cards}
        }
    Player3 ->
      Players
        { player1 = (g ^. #players . #player1){cards = player1Cards}
        , player2 = (g ^. #players . #player2){cards = player2Cards}
        , player3 = (g ^. #players . #player3){cards = hakemCards}
        , player4 = (g ^. #players . #player4){cards = player3Cards}
        }
    Player4 ->
      Players
        { player1 = (g ^. #players . #player1){cards = player1Cards}
        , player2 = (g ^. #players . #player2){cards = player2Cards}
        , player3 = (g ^. #players . #player3){cards = player3Cards}
        , player4 = (g ^. #players . #player4){cards = hakemCards}
        }
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
     in GameEndOfRound <| GameEndOfRoundState{board = Four first second third newCard, ..}
 where
  newCard = PlayedCard card idx
addPlayerCardToBoard idx card g = g

nextTurn :: Game -> Game
nextTurn = _GameInProgress . #turn %~ nextPlayerIndexTurn
