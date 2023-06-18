{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hokm.Hedgehog.Gen.Game where

import Data.List.Split (chunksOf)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Soltan.Data.AtMostThree as AtMostThree
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Username as Username
import Soltan.Hokm
import Soltan.Hokm.Types hiding (username)
import Soltan.Hokm.Utils (nextPlayerIndexTurn, splitFourWay)
import Prelude hiding (state)

genShuffledCards :: Int -> Gen [Card]
genShuffledCards n = do
  cs <- Gen.shuffle initialDeck
  pure <| take n cs

genShuffledDeck :: Gen [Card]
genShuffledDeck = Gen.shuffle initialDeck

genSuit :: Gen Suit
genSuit = Gen.enum Diamonds Spades

genUsername :: Gen Username
genUsername =
  Username.UnsafeMk
    <$> Gen.text
      (Range.linear 8 20)
      (Gen.choice [Gen.hexit, pure '_'])

genPlayerIndex :: Gen PlayerIndex
genPlayerIndex = Gen.enumBounded

genPlayerIndexExcept :: PlayerIndex -> Gen PlayerIndex
genPlayerIndexExcept idx = Gen.element (filter (/= idx) [Player1, Player2, Player3, Player4])

genPlayer :: Team -> [Card] -> Gen Player
genPlayer team cards = do
  playerName <- genUsername
  pure Player{..}

genChoosingHokmGame :: Gen (Game, ChoosingHokmState)
genChoosingHokmGame = do
  deck <- genShuffledDeck
  let (hakemCards, remainingDeck) = splitAt 5 deck
  hakem <- genPlayerIndex
  let getCards idx = if idx == hakem then hakemCards else []
  p1 <- genPlayer A (getCards Player1)
  p2 <- genPlayer B (getCards Player2)
  p3 <- genPlayer A (getCards Player3)
  p4 <- genPlayer B (getCards Player4)
  teamAPoints <- Point . fromIntegral <$> Gen.int (Range.constant 0 5)
  teamBPoints <- Point . fromIntegral <$> Gen.int (Range.constant 0 5)
  let players = Players p1 p2 p3 p4
  let state = ChoosingHokmState{..}
  pure (GameChoosingHokm state, state)

data GenerateGameInProgressInfo = GenerateGameInProgressInfo
  { player1Cards :: [Card]
  , player2Cards :: [Card]
  , player3Cards :: [Card]
  , player4Cards :: [Card]
  , turn :: PlayerIndex
  , board :: [PlayedCard]
  }

genInProgressGame :: GenerateGameInProgressInfo -> Gen (Game, GameInProgressState)
genInProgressGame GenerateGameInProgressInfo{..} = do
  hakem <- genPlayerIndex
  trumpSuit <- genSuit
  p1 <- genPlayer A player1Cards
  p2 <- genPlayer B player2Cards
  p3 <- genPlayer A player3Cards
  p4 <- genPlayer B player4Cards
  teamAPoints <- Point . fromIntegral <$> Gen.int (Range.constant 0 5)
  teamBPoints <- Point . fromIntegral <$> Gen.int (Range.constant 0 5)
  teamATricks <- Trick . fromIntegral <$> Gen.int (Range.constant 0 5)
  teamBTricks <- Trick . fromIntegral <$> Gen.int (Range.constant 0 5)
  let players = Players p1 p2 p3 p4
  let gameBoard = case board of
        [c1, c2, c3] -> AtMostThree.Three c1 c2 c3
        [c1, c2] -> AtMostThree.Two c1 c2
        [c1] -> AtMostThree.One c1
        _ -> AtMostThree.Zero
  let state = GameInProgressState{board = gameBoard, ..}
  pure (GameInProgress state, state)

genInProgressGame_ :: Gen (Game, GameInProgressState)
genInProgressGame_ = do
  numCards <- Gen.element (map (* 4) [1 .. 13])
  cards <- genShuffledCards numCards
  boardCardsCount <- Gen.int (Range.constant 0 3)
  let (boardCards, hands) = splitAt boardCardsCount cards
  let (player1Cards, player2Cards, player3Cards, player4Cards) = splitFourWay hands
  -- always player 1 starts
  let board = fmap (uncurry PlayedCard) <| zip boardCards [Player1, Player2, Player3]
  let turn = fromMaybe Player1 <| fmap nextPlayerIndexTurn <| fmap (view #playerIndex) <| viaNonEmpty last <| board
  genInProgressGame GenerateGameInProgressInfo{..}
