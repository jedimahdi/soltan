module Test.Hokm.HokmSpec where

import Control.Lens ((^..), (^?))
import Hokm.Utils (initialTestPlayers)
import qualified Soltan.Data.AtMostThree as AtMostThree
import qualified Soltan.Data.Four as Four
import Soltan.Hokm.Hokm (nextStage)
import Soltan.Hokm.Types (Card (..), Game (..), GameEndOfTrickState (..), PlayedCard (..), PlayerIndex (..), Rank (..), Suit (..), Team (..), _GameChoosingHokm, _GameEnd, _GameInProgress)
import Soltan.Hokm.Utils
import System.Random (getStdGen)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (state)

spec :: Spec
spec = describe "nextStage" do
  let initialState hakem teamAPoints teamBPoints teamATricks teamBTricks =
        GameEndOfTrickState
          { players = initialTestPlayers [] [] [] []
          , teamAPoints
          , teamBPoints
          , teamATricks
          , teamBTricks
          , hakem
          , trumpSuit = Spades
          , board =
              Four.Four
                (PlayedCard (Card Jack Diamonds) Player2)
                (PlayedCard (Card King Diamonds) Player3)
                (PlayedCard (Card Three Spades) Player4)
                (PlayedCard (Card Seven Diamonds) Player1)
          }
  it "should go end when some team reaches 7 points" do
    gen <- getStdGen
    let state = initialState Player1 0 6 0 6
    let newGame = nextStage gen (GameEndOfTrick state)
    newGame ^? _GameEnd . #winnerTeam `shouldBe` Just B

  it "should go next round when a team tricks reaches 7" do
    gen <- getStdGen
    let state = initialState Player1 0 0 0 6
    let newGame = nextStage gen (GameEndOfTrick state)
    newGame ^? _GameChoosingHokm . #hakem `shouldBe` Just Player2

  it "hakem should not change when hakem team wins" do
    gen <- getStdGen
    let state = initialState Player4 0 0 0 6
    let newGame = nextStage gen (GameEndOfTrick state)
    newGame ^? _GameChoosingHokm . #hakem `shouldBe` Just Player4

  it "should go next trick when game and round is not ended" do
    gen <- getStdGen
    let state = initialState Player1 0 0 0 0
    let newGame = nextStage gen (GameEndOfTrick state)
    newGame ^? _GameInProgress . #board `shouldBe` Just AtMostThree.Zero
