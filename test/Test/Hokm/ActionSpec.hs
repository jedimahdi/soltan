module Test.Hokm.ActionSpec where

import Control.Exception (throw)
import Control.Lens (element, lengthOf, to, (%~), (&), (.~), (?~), (^..), (^?))
import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Hedgehog (Property, assert, evalEither, evalMaybe, forAll, property, success)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hokm.Hedgehog.Gen.Game
import Hokm.Utils
import qualified Soltan.Data.Four as Four
import qualified Soltan.Data.Username as Username
import Soltan.Hokm.Action
import Soltan.Hokm.ActionValidation
import Soltan.Hokm.Types
import Soltan.Hokm.Utils (findWinnerOfTrick, getBaseSuit, getBaseSuitEndOfTrick, initialDeck, mkPlayers, playersToList)
import Test.Hspec (Spec, before, describe, it, shouldBe, shouldContain)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))

choosingHokmFixture :: Game
choosingHokmFixture =
  GameChoosingHokm
    <| ChoosingHokmState
      { remainingDeck =
          (Card <$> [minBound ..] <*> [minBound ..])
            \\ [Card Three Spades, Card Ace Hearts, Card King Diamonds, Card Four Diamonds, Card Two Diamonds]
      , hakem = Player2
      , players =
          initialTestPlayers
            []
            [Card Three Spades, Card Ace Hearts, Card King Diamonds, Card Four Diamonds, Card Two Diamonds]
            []
            []
      , teamAPoints = 0
      , teamBPoints = 0
      }

isInProgressState :: Game -> Bool
isInProgressState (GameInProgress _) = True
isInProgressState _ = False

rankAndSuitCardOrdering :: Card -> Card -> Ordering
rankAndSuitCardOrdering (Card rank1 suit1) (Card rank2 suit2) = case compare suit1 suit2 of
  EQ -> compare rank1 rank2
  c -> c

spec :: Spec
spec = describe "runAction" do
  it "ChooseHokm Action" do
    hedgehog do
      (game, state) <- forAll genChoosingHokmGame
      choosenTrumpSuit <- forAll genSuit
      let hakem = state ^. #hakem
      let action = ChooseHokm hakem choosenTrumpSuit
      newGame <- evalEither <| runAction action game
      newGameState <- evalMaybe <| newGame ^? _GameInProgress
      newGameState ^. #turn === hakem
      let players = newGameState ^. #players . to playersToList
      forM_ players \player -> do
        lengthOf (#cards . traverse) player === 13
      lengthOf (traverse . #cards . traverse) players === 52
      sortBy rankAndSuitCardOrdering (players ^.. traverse . #cards . traverse) === sortBy rankAndSuitCardOrdering initialDeck
      newGameState ^. #teamAPoints === state ^. #teamAPoints
      newGameState ^. #teamBPoints === state ^. #teamBPoints
      newGameState ^. #teamATricks === 0
      newGameState ^. #teamBTricks === 0
  it "PlayCard Action" do
    hedgehog do
      (game, state) <- forAll genInProgressGame_
      let turn = state ^. #turn
      cardToPlay <- forAll case getBaseSuit state of
        Nothing -> Gen.element <| state ^.. #players . playerL turn . #cards . traverse
        Just baseSuit -> pure (Card Five Diamonds)
      let action = PlayCard turn
      pass
