module Test.Hokm.ActionSpec where

import Control.Exception (throw)
import Control.Lens (element, lengthOf, to, (%~), (&), (.~), (?~), (^..), (^?))
import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Hedgehog (Property, assert, evalEither, evalMaybe, forAll, property, success)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hokm.Hedgehog.Gen.Game
import qualified Soltan.Data.Username as Username
import Soltan.Hokm.Action
import Soltan.Hokm.ActionValidation
import Soltan.Hokm.Types
import Soltan.Hokm.Utils (initialDeck, playersToList)
import Test.Hspec (Spec, before, describe, it, shouldBe, shouldContain)
import Test.Hspec.Hedgehog (forAll, hedgehog, (/==), (===))
import Prelude hiding (state)

choosingHokmFixture :: Game
choosingHokmFixture =
  GameChoosingHokm
    <| ChoosingHokmState
      { remainingDeck =
          (Card <$> [minBound ..] <*> [minBound ..])
            \\ [Card Three Spades, Card Ace Hearts, Card King Diamonds, Card Four Diamonds, Card Two Diamonds]
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

rankAndSuitCardOrdering :: Card -> Card -> Ordering
rankAndSuitCardOrdering (Card rank1 suit1) (Card rank2 suit2) = case compare suit1 suit2 of
  EQ -> compare rank1 rank2
  c -> c

spec :: Spec
spec = describe "runAction" do
  it "ChooseHokm Action" do
    hedgehog do
      (game, state) <- forAll genChoosingHokmGame
      trumpSuit <- forAll genSuit
      let hakem = state ^. #hakem
      action <- evalEither <| validateAction game (ChooseHokm hakem trumpSuit)
      let newGame = runAction action game
      newGameState <- evalMaybe <| newGame ^? _GameInProgress
      newGameState ^. #turn === hakem
      let players = newGameState ^. #players . to playersToList
      forM_ players \player -> do
        lengthOf (#cards . traverse) player === 13
      lengthOf (traverse . #cards . traverse) players === 52
      sortBy rankAndSuitCardOrdering (players ^.. traverse . #cards . traverse) === sortBy rankAndSuitCardOrdering initialDeck
      newGameState ^. #teamAPoints === 0
      newGameState ^. #teamBPoints === 0
      newGameState ^. #teamARounds === 0
      newGameState ^. #teamBRounds === 0
