module Test.Hokm.ActionValidationSpec where

import Control.Lens (element, (%~), (&), (.~), (?~))
import Data.Either (isLeft, isRight)
import Hedgehog (Property, forAll, property, withDiscards, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hokm.Hedgehog.Gen.Game
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
          Players
            { player1 =
                Player
                  { playerName = Username.UnsafeMk "player1"
                  , team = A
                  , cards = []
                  }
            , player2 =
                Player
                  { playerName = Username.UnsafeMk "player2"
                  , team = B
                  , cards = [Card Three Spades, Card Ace Hearts, Card King Diamonds, Card Four Diamonds, Card Two Diamonds]
                  }
            , player3 =
                Player
                  { playerName = Username.UnsafeMk "player3"
                  , team = A
                  , cards = []
                  }
            , player4 =
                Player
                  { playerName = Username.UnsafeMk "player4"
                  , team = B
                  , cards = []
                  }
            }
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
          Players
            { player1 =
                Player
                  { playerName = Username.UnsafeMk "player1"
                  , team = A
                  , cards = [Card Four Diamonds, Card Two Diamonds]
                  }
            , player2 =
                Player
                  { playerName = Username.UnsafeMk "player2"
                  , team = B
                  , cards = [Card Three Spades, Card Ace Hearts]
                  }
            , player3 =
                Player
                  { playerName = Username.UnsafeMk "player3"
                  , team = A
                  , cards = [Card Three Clubs, Card King Hearts]
                  }
            , player4 =
                Player
                  { playerName = Username.UnsafeMk "player4"
                  , team = B
                  , cards = [Card Queen Clubs, Card Ace Clubs]
                  }
            }
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
          Players
            { player1 =
                Player
                  { playerName = Username.UnsafeMk "player1"
                  , team = A
                  , cards = [Card Four Diamonds, Card Three Spades]
                  }
            , player2 =
                Player
                  { playerName = Username.UnsafeMk "player2"
                  , team = B
                  , cards = [Card Two Diamonds, Card Ace Hearts]
                  }
            , player3 =
                Player
                  { playerName = Username.UnsafeMk "player3"
                  , team = A
                  , cards = [Card Three Clubs]
                  }
            , player4 =
                Player
                  { playerName = Username.UnsafeMk "player4"
                  , team = B
                  , cards = [Card Queen Clubs]
                  }
            }
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
      , players =
          Players
            { player1 =
                Player
                  { playerName = Username.UnsafeMk "player1"
                  , team = A
                  , cards = []
                  }
            , player2 =
                Player
                  { playerName = Username.UnsafeMk "player2"
                  , team = B
                  , cards = []
                  }
            , player3 =
                Player
                  { playerName = Username.UnsafeMk "player3"
                  , team = A
                  , cards = []
                  }
            , player4 =
                Player
                  { playerName = Username.UnsafeMk "player4"
                  , team = B
                  , cards = []
                  }
            }
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
        validateAction game action === Right (unsafeStatusCoerce action)

  describe "PlayCard Action" do
    it "should return OutOfTurn error when it is wrong turn" do
      let action = PlayCard Player2 (Card Two Diamonds)
      validateAction gameInProgresFixture action `shouldBe` Left (OutOfTurn Player1)

    it "should return validated Action if player can play the card" do
      let action = PlayCard Player1 (Card Three Spades)
      validateAction gameInProgresFixture action `shouldBe` Right (unsafeStatusCoerce action)

  describe "NextRound Action" do
    it "should return validated Action if all players played their card in the round" do
      let action = NextRound
      validateAction gameEndOfRoundFixture action `shouldBe` Right (unsafeStatusCoerce action)
