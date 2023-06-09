{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hokm.Hedgehog.Gen.Game where

import Data.List.Split (chunksOf)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Username as Username
import Soltan.Hokm
import Soltan.Hokm.Types hiding (username)
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

-- players :: [Card] -> Gen Players
-- players cards = do
--   let [c1, c2, c3, c4] = chunksOf 13 cards
--   p1 <- player A c1
--   p2 <- player B c2
--   p3 <- player A c3
--   p4 <- player B c4
--   pure (Players p1 p2 p3 p4)

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
  let players = Players p1 p2 p3 p4
  let state = ChoosingHokmState{..}
  pure <| (GameChoosingHokm state, state)
