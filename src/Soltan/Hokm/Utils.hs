module Soltan.Hokm.Utils where

import Control.Lens (anyOf, elemOf, lengthOf, traversed, (^..))
import Data.List (foldl1', (!!))
import Soltan.Data.AtMostThree (AtMostThree (..))
import Soltan.Data.Username (Username)
import Soltan.Hokm.Types hiding (Three, Two)

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
getPlayers GameBeforeStart = Nothing
getPlayers (GameChoosingHokm g) = Just <| g ^. #players
getPlayers (GameInProgress g) = Just <| g ^. #players
getPlayers (GameEndOfRound g) = Just <| g ^. #players
getPlayers (GameEnd g) = Just <| g ^. #players

getPlayerIndexWithUsername' :: Username -> Players -> Maybe PlayerIndex
getPlayerIndexWithUsername' username (Players player1 player2 player3 player4)
  | player1 ^. #playerName == username = Just Player1
  | player2 ^. #playerName == username = Just Player2
  | player3 ^. #playerName == username = Just Player3
  | player4 ^. #playerName == username = Just Player4
  | otherwise = Nothing

getPlayerIndexWithUsername :: Username -> Game -> Maybe PlayerIndex
getPlayerIndexWithUsername username game = getPlayers game >>= getPlayerIndexWithUsername' username
