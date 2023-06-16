module Test.Hokm.ActionValidationSpec where

import Control.Lens (element, (%~), (&), (.~), (?~))
import Data.Either (isLeft, isRight)
import Hedgehog (Property, forAll, property, withDiscards, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hokm.Hedgehog.Gen.Game
import Hokm.Utils (initialTestPlayers)
import qualified Soltan.Data.AtMostThree as AtMostThree
import qualified Soltan.Data.Four as Four
import qualified Soltan.Data.Username as Username
import Soltan.Hokm.ActionValidation
import Soltan.Hokm.Types
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))
import Prelude hiding (state)

initialGame :: Game
initialGame = GameBeforeStart

choosingHokmFixture :: Game
choosingHokmFixture =
  GameChoosingHokm
    <| ChoosingHokmState
      { remainingDeck = []
      , hakem = Player2
      , players =
          initialTestPlayers
            []
            [Card Three Spades, Card Ace Hearts, Card King Diamonds, Card Four Diamonds, Card Two Diamonds]
            []
            []
      }

gameEndOfRoundFixture :: Game
gameEndOfRoundFixture =
  GameEndOfRound
    <| GameEndOfRoundState
      { hakem = Player2
      , trumpSuit = Clubs
      , board =
          Four.Four
            (PlayedCard (Card Six Spades) Player2)
            (PlayedCard (Card Five Spades) Player1)
            (PlayedCard (Card Two Spades) Player4)
            (PlayedCard (Card Queen Spades) Player3)
      , teamAPoints = 0
      , teamBPoints = 0
      , teamARounds = 0
      , teamBRounds = 0
      , players =
          initialTestPlayers
            [Card Four Diamonds, Card Two Diamonds]
            [Card Three Spades, Card Ace Hearts]
            [Card Three Clubs, Card King Hearts]
            [Card Queen Clubs, Card Ace Clubs]
      }

gameInProgresFixture :: Game
gameInProgresFixture =
  GameInProgress
    <| GameInProgressState
      { hakem = Player2
      , turn = Player1
      , trumpSuit = Clubs
      , board =
          AtMostThree.Two
            ( PlayedCard
                (Card Two Spades)
                Player4
            )
            ( PlayedCard
                (Card Queen Spades)
                Player3
            )
      , teamAPoints = 0
      , teamBPoints = 0
      , teamARounds = 0
      , teamBRounds = 0
      , players =
          initialTestPlayers
            [Card Four Diamonds, Card Three Spades]
            [Card Two Diamonds, Card Ace Hearts]
            [Card Three Clubs]
            [Card Queen Clubs]
      }

gameEndFixture :: Game
gameEndFixture =
  GameEnd
    <| GameEndState
      { winnerTeam = A
      , teamAPoints = 1
      , teamBPoints = 0
      , teamARounds = 0
      , teamBRounds = 0
      , prevHakem = Player2
      , players = initialTestPlayers [] [] [] []
      }

spec :: Spec
spec = describe "validateAction" do
  describe "ChooseHokm Action" do
    it "should return NoChoosingHokmPhase error if game is GameBeforeStart" do
      let action = ChooseHokm Player2 Diamonds
      validateAction initialGame action `shouldBe` Left NoChoosingHokmPhase

    it "should return NotHakem when player who is choosing is not Hakem" do
      hedgehog do
        (game, state) <- forAll genChoosingHokmGame
        trumpSuit <- forAll genSuit
        notHakem <- forAll <| genPlayerIndexExcept (state ^. #hakem)
        let action = ChooseHokm notHakem trumpSuit
        validateAction game action === Left NotHakem

    it "should not return an error if player can choose hokm" do
      hedgehog do
        (game, state) <- forAll genChoosingHokmGame
        trumpSuit <- forAll genSuit
        let action = ChooseHokm (state ^. #hakem) trumpSuit
        validateAction game action === Right ()

  describe "PlayCard Action" do
    it "should return OutOfTurn error when it is wrong turn" do
      let action = PlayCard Player2 (Card Two Diamonds)
      validateAction gameInProgresFixture action `shouldBe` Left (OutOfTurn Player1)

    it "should return validated Action if player can play the card" do
      let action = PlayCard Player1 (Card Three Spades)
      validateAction gameInProgresFixture action `shouldBe` Right ()

  describe "NextRound Action" do
    it "should return validated Action if all players played their card in the round" do
      let action = NextRound
      validateAction gameEndOfRoundFixture action `shouldBe` Right ()
