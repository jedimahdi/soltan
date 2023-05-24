module Test.Hokm.ActionSpec where

import Control.Exception (throw)
import Control.Lens (element, lengthOf, (%~), (&), (.~), (?~), (^?))
import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Hedgehog (Property, forAll, property, withDiscards, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Soltan.Data.Username as Username
import Soltan.Hokm.Action
import Soltan.Hokm.ActionValidation
import Soltan.Hokm.Types
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Test.Hspec.Hedgehog (
  PropertyT,
  diff,
  forAll,
  hedgehog,
  (/==),
  (===),
 )

choosingHokmFixture :: Game
choosingHokmFixture =
  GameChoosingHokm
    <| ChoosingHokmState
      { remainingDeck = (Card <$> [minBound ..] <*> [minBound ..]) \\ [Card Three Spades, Card Ace Hearts, Card King Diamonds, Card Four Diamonds, Card Two Diamonds]
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

isInProgressState :: Game -> Bool
isInProgressState (GameInProgress _) = True
isInProgressState _ = False

-- throwLeft :: Exception e => Either e a -> a
-- throwLeft (Left e) = throw e
-- throwLeft (Right a) = a

spec :: Spec
spec = do
  describe "runAction" do
    describe "ChooseHokm Action" do
      it "should update the game to inital InProgressGame" do
        let game = choosingHokmFixture
        let newGame = chooseHokm Player2 Hearts game
        isInProgressState newGame `shouldBe` True

      it "should be Hakem's turn" do
        let game = choosingHokmFixture
        let hakem = game ^? _GameChoosingHokm . #hakem
        let newGame = chooseHokm Player2 Hearts game
        newGame ^? _GameInProgress . #turn `shouldBe` hakem

      it "players should each have 13 cards" do
        let game = choosingHokmFixture
        let hakem = game ^? _GameChoosingHokm . #hakem
        let newGame = chooseHokm Player2 Hearts game
        lengthOf (_GameInProgress . #players . #player1 . #cards . traverse) newGame `shouldBe` 13
        lengthOf (_GameInProgress . #players . #player2 . #cards . traverse) newGame `shouldBe` 13
        lengthOf (_GameInProgress . #players . #player3 . #cards . traverse) newGame `shouldBe` 13
        lengthOf (_GameInProgress . #players . #player4 . #cards . traverse) newGame `shouldBe` 13
