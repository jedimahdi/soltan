module Soltan.Hokm.Utils where

import Control.Lens (anyOf, elemOf, lengthOf, traversed, (^..))
import Data.List (foldl, foldl1', head, tail)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Soltan.Data.AtMostThree (AtMostThree (..))
import qualified Soltan.Data.Four as Four
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Username as Username
import Soltan.Hokm.Types hiding (Three, Two)
import System.Random (Random (randomR), RandomGen)
import Prelude hiding (head, tail)

mkPlayers :: (Username, [Card]) -> (Username, [Card]) -> (Username, [Card]) -> (Username, [Card]) -> Players
mkPlayers (u1, c1) (u2, c2) (u3, c3) (u4, c4) =
  Players
    { player1 =
        ( Player
            { playerName = u1
            , team = A
            , cards = c1
            }
        )
    , player2 =
        ( Player
            { playerName = u2
            , team = B
            , cards = c2
            }
        )
    , player3 =
        ( Player
            { playerName = u3
            , team = A
            , cards = c3
            }
        )
    , player4 =
        ( Player
            { playerName = u4
            , team = B
            , cards = c4
            }
        )
    }
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

getBaseSuitEndOfTrick :: GameEndOfTrickState -> Suit
getBaseSuitEndOfTrick g = g ^. #board . #first . #card . #suit

isEndOfTrick :: Game -> Bool
isEndOfTrick (GameEndOfTrick _) = True
isEndOfTrick _ = False

haveSuit :: GameInProgressState -> PlayerIndex -> Suit -> Bool
haveSuit g idx suit = elemOf (#players . playerL idx . #cards . traversed . #suit) suit g

highestOfSuit :: Suit -> [PlayedCard] -> PlayerIndex
highestOfSuit suit =
  view #playerIndex
    . foldl1' (\p1@(PlayedCard card1 _) p2@(PlayedCard card2 _) -> if card1 >= card2 then p1 else p2)
    . filter (\p -> p ^. #card . #suit == suit)

findWinnerOfTrick :: GameEndOfTrickState -> PlayerIndex
findWinnerOfTrick g
  | elemOf (#board . traversed . #card . #suit) (g ^. #trumpSuit) g = highestOfSuit (g ^. #trumpSuit) (g ^.. #board . traverse)
  | otherwise = highestOfSuit (getBaseSuitEndOfTrick g) (g ^.. #board . traverse)

nextPlayerIndexTurn :: PlayerIndex -> PlayerIndex
nextPlayerIndexTurn Player1 = Player2
nextPlayerIndexTurn Player2 = Player3
nextPlayerIndexTurn Player3 = Player4
nextPlayerIndexTurn Player4 = Player1

getPlayers :: Game -> Maybe Players
getPlayers (GameChoosingHokm g) = Just <| g ^. #players
getPlayers (GameInProgress g) = Just <| g ^. #players
getPlayers (GameEndOfTrick g) = Just <| g ^. #players
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
      , teamATricks = 0
      , teamBTricks = 0
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
      , teamATricks = 0
      , teamBTricks = 0
      , teamAPoints = s ^. #teamAPoints
      , teamBPoints = s ^. #teamBPoints
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
      , teamATricks = s ^. #teamATricks
      , teamBTricks = s ^. #teamBTricks
      , teamAPoints = s ^. #teamAPoints
      , teamBPoints = s ^. #teamBPoints
      }
mkGameSummary username game@(GameEndOfTrick s) = do
  idx <- getPlayerIndexWithUsername username game
  let players = s ^. #players
  let player = players ^. playerL idx
  let cards = player ^. #cards
  let playersSummary = mkPlayersSummary players
  let hakem = getPlayerName (s ^. #hakem) players
  let board = s ^. #board |> toList
  pure
    GameSummary
      { status = SummaryEndOfTrick
      , cards
      , players = playersSummary
      , hakem = Just hakem
      , trumpSuit = Just (s ^. #trumpSuit)
      , turn = Nothing
      , board
      , teamATricks = s ^. #teamATricks
      , teamBTricks = s ^. #teamBTricks
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
      { status = SummaryEndOfTrick
      , cards
      , players = playersSummary
      , hakem = Nothing
      , trumpSuit = Nothing
      , turn = Nothing
      , board = []
      , teamATricks = 0
      , teamBTricks = 0
      , teamAPoints = s ^. #teamAPoints
      , teamBPoints = s ^. #teamBPoints
      }

splitThreeWayWithRem :: Int -> [a] -> ([a], [a], [a], [a])
splitThreeWayWithRem n xs =
  let r = (length xs + n) `quot` 4
   in ( take r xs
      , take r (drop r xs)
      , take r (drop (2 * r) xs)
      , drop (3 * r) xs
      )

splitFourWay :: [a] -> ([a], [a], [a], [a])
splitFourWay xs =
  let r = length xs `quot` 4
   in ( take r xs <> take 1 (drop (4 * r) xs)
      , take r (drop r xs) <> take 1 (drop (4 * r + 1) xs)
      , take r (drop (2 * r) xs) <> take 1 (drop (4 * r + 2) xs)
      , take r (drop (3 * r) xs)
      )

initialDeck :: [Card]
initialDeck = Card <$> [minBound ..] <*> [minBound ..]

-- Get a shuffled deck of cards.
shuffledDeck :: RandomGen g => g -> [Card]
shuffledDeck gen = fst <| shuffle gen initialDeck

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) =
  ((M.insert j x . M.insert i (m M.! j)) m, gen')
 where
  (j, gen') = randomR (0, i) gen

-- shuffle using the Fisher Yates algorithm
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen l =
  toElems $
    foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
 where
  toElems (x, y) = (M.elems x, y)
  numerate = zip [1 ..]
  initial x gen' = (M.singleton 0 x, gen')
