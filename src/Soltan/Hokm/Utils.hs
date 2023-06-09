module Soltan.Hokm.Utils where

import Control.Lens (anyOf, elemOf, lengthOf, traversed, (^..))
import Data.List (foldl1')
import Soltan.Data.AtMostThree (AtMostThree (..))
import Soltan.Data.Username (Username)
import Soltan.Hokm.Types hiding (Three, Two)

initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

haveCard :: Game -> PlayerIndex -> Card -> Bool
haveCard (GameInProgress g) idx card
  | elemOf (#players . playerL idx . #cards . traversed) card g = True
  | otherwise = False
haveCard _ _ _ = False

getBaseSuit :: GameInProgressState -> Maybe Suit
getBaseSuit g = case g ^. #board of
  Zero -> Nothing
  One x -> Just <| x ^. #card . #suit
  Two x _ -> Just <| x ^. #card . #suit
  Three x _ _ -> Just <| x ^. #card . #suit

getBaseSuitEndOfRound :: GameEndOfRoundState -> Suit
getBaseSuitEndOfRound g = g ^. #board . #first . #card . #suit

haveSuit :: GameInProgressState -> PlayerIndex -> Suit -> Bool
haveSuit g idx suit = elemOf (#players . playerL idx . #cards . traversed . #suit) suit g

highestOfSuit :: Suit -> [PlayedCard] -> PlayerIndex
highestOfSuit suit =
  view #playerIndex
    . foldl1' (\p1@(PlayedCard card1 _) p2@(PlayedCard card2 _) -> if card1 >= card2 then p1 else p2)
    . filter (\p -> p ^. #card . #suit == suit)

findWinnerOfRound :: GameEndOfRoundState -> PlayerIndex
findWinnerOfRound g
  | anyOf (#board . traversed . #card . #suit) (== (g ^. #trumpSuit)) g = highestOfSuit (g ^. #trumpSuit) (g ^.. #board . traverse)
  | otherwise = highestOfSuit (getBaseSuitEndOfRound g) (g ^.. #board . traverse)

nextPlayerIndexTurn :: PlayerIndex -> PlayerIndex
nextPlayerIndexTurn Player1 = Player2
nextPlayerIndexTurn Player2 = Player3
nextPlayerIndexTurn Player3 = Player4
nextPlayerIndexTurn Player4 = Player1

getPlayers :: Game -> Maybe Players
getPlayers (GameChoosingHokm g) = Just <| g ^. #players
getPlayers (GameInProgress g) = Just <| g ^. #players
getPlayers (GameEndOfRound g) = Just <| g ^. #players
getPlayers (GameEnd g) = Just <| g ^. #players
getPlayers _ = Nothing

getPlayerIndexWithUsername' :: Username -> Players -> Maybe PlayerIndex
getPlayerIndexWithUsername' username (Players player1 player2 player3 player4)
  | player1 ^. #playerName == username = Just Player1
  | player2 ^. #playerName == username = Just Player2
  | player3 ^. #playerName == username = Just Player3
  | player4 ^. #playerName == username = Just Player4
  | otherwise = Nothing

getPlayerIndexWithUsername :: Username -> Game -> Maybe PlayerIndex
getPlayerIndexWithUsername username game = getPlayers game >>= getPlayerIndexWithUsername' username

getPlayerName :: PlayerIndex -> Players -> Username
getPlayerName idx players = players ^. playerL idx . #playerName

playersToList :: Players -> [Player]
playersToList (Players p1 p2 p3 p4) = [p1, p2, p3, p4]

mkPlayersSummary :: Players -> [PlayerSummary]
mkPlayersSummary (Players p1 p2 p3 p4) =
  [ PlayerSummary (p1 ^. #playerName) (p1 ^. #team) Player1
  , PlayerSummary (p2 ^. #playerName) (p2 ^. #team) Player2
  , PlayerSummary (p3 ^. #playerName) (p3 ^. #team) Player3
  , PlayerSummary (p4 ^. #playerName) (p4 ^. #team) Player4
  ]

mkGameSummary :: Username -> Game -> Maybe GameSummary
mkGameSummary _ GameBeforeStart =
  pure
    GameSummary
      { status = SummaryNotStarted
      , cards = []
      , players = []
      , hakem = Nothing
      , trumpSuit = Nothing
      , turn = Nothing
      , board = []
      , teamARounds = 0
      , teamBRounds = 0
      , teamAPoints = 0
      , teamBPoints = 0
      }
mkGameSummary username game@(GameChoosingHokm s) = do
  idx <- getPlayerIndexWithUsername username game
  let players = s ^. #players
  let player = players ^. playerL idx
  let cards = player ^. #cards
  let playersSummary = mkPlayersSummary players
  let hakem = getPlayerName (s ^. #hakem) players
  pure
    GameSummary
      { status = SummaryChoosingHokm
      , cards
      , players = playersSummary
      , hakem = Just hakem
      , trumpSuit = Nothing
      , turn = Nothing
      , board = []
      , teamARounds = 0
      , teamBRounds = 0
      , teamAPoints = 0
      , teamBPoints = 0
      }
mkGameSummary username game@(GameInProgress s) = do
  idx <- getPlayerIndexWithUsername username game
  let players = s ^. #players
  let player = players ^. playerL idx
  let cards = player ^. #cards
  let playersSummary = mkPlayersSummary players
  let hakem = getPlayerName (s ^. #hakem) players
  let board = s ^. #board |> toList
  let turn = getPlayerName (s ^. #turn) players
  pure
    GameSummary
      { status = SummaryInProgress
      , cards
      , players = playersSummary
      , hakem = Just hakem
      , trumpSuit = Just (s ^. #trumpSuit)
      , turn = Just turn
      , board
      , teamARounds = s ^. #teamARounds
      , teamBRounds = s ^. #teamBRounds
      , teamAPoints = s ^. #teamAPoints
      , teamBPoints = s ^. #teamBPoints
      }
mkGameSummary username game@(GameEndOfRound s) = do
  idx <- getPlayerIndexWithUsername username game
  let players = s ^. #players
  let player = players ^. playerL idx
  let cards = player ^. #cards
  let playersSummary = mkPlayersSummary players
  let hakem = getPlayerName (s ^. #hakem) players
  let board = s ^. #board |> toList
  pure
    GameSummary
      { status = SummaryEndOfRound
      , cards
      , players = playersSummary
      , hakem = Just hakem
      , trumpSuit = Just (s ^. #trumpSuit)
      , turn = Nothing
      , board
      , teamARounds = s ^. #teamARounds
      , teamBRounds = s ^. #teamBRounds
      , teamAPoints = s ^. #teamAPoints
      , teamBPoints = s ^. #teamBPoints
      }
mkGameSummary username game@(GameEnd s) = do
  idx <- getPlayerIndexWithUsername username game
  let players = s ^. #players
  let player = players ^. playerL idx
  let cards = player ^. #cards
  let playersSummary = mkPlayersSummary players
  pure
    GameSummary
      { status = SummaryEndOfRound
      , cards
      , players = playersSummary
      , hakem = Nothing
      , trumpSuit = Nothing
      , turn = Nothing
      , board = []
      , teamARounds = s ^. #teamARounds
      , teamBRounds = s ^. #teamBRounds
      , teamAPoints = s ^. #teamAPoints
      , teamBPoints = s ^. #teamBPoints
      }
