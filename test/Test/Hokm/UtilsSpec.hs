module Test.Hokm.UtilsSpec where

import Control.Lens ((^..))
import Hokm.Utils (initialTestPlayers)
import qualified Soltan.Data.Four as Four
import Soltan.Hokm.Types (Card (..), GameEndOfTrickState (..), PlayedCard (..), PlayerIndex (..), Rank (..), Suit (..))
import Soltan.Hokm.Utils
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "findWinnerOfTrick" do
  let initialState (c1, i1) (c2, i2) (c3, i3) (c4, i4) =
        GameEndOfTrickState
          { players = initialTestPlayers [] [] [] []
          , teamAPoints = 0
          , teamBPoints = 0
          , teamATricks = 0
          , teamBTricks = 0
          , hakem = Player1
          , trumpSuit = Spades
          , board =
              Four.Four
                (PlayedCard c1 i1)
                (PlayedCard c2 i2)
                (PlayedCard c3 i3)
                (PlayedCard c4 i4)
          }
  it "winner should be hightest card value of same suit" do
    let state =
          initialState
            (Card Jack Diamonds, Player4)
            (Card King Diamonds, Player1)
            (Card Three Diamonds, Player2)
            (Card Seven Diamonds, Player3)

    state ^.. #board . traverse
      `shouldBe` [ PlayedCard (Card Jack Diamonds) Player4
                 , PlayedCard (Card King Diamonds) Player1
                 , PlayedCard (Card Three Diamonds) Player2
                 , PlayedCard (Card Seven Diamonds) Player3
                 ]
    getBaseSuitEndOfTrick state `shouldBe` Diamonds
    findWinnerOfTrick state `shouldBe` Player1

  it "winner should be the one with trump suit card" do
    let state =
          initialState
            (Card Jack Diamonds, Player2)
            (Card King Diamonds, Player3)
            (Card Three Spades, Player4)
            (Card Seven Diamonds, Player1)
    findWinnerOfTrick state `shouldBe` Player4

  it "winner should be the one with highest of trump suit card" do
    let state =
          initialState
            (Card Ace Hearts, Player3)
            (Card Three Hearts, Player4)
            (Card Three Spades, Player1)
            (Card Four Spades, Player2)
    findWinnerOfTrick state `shouldBe` Player2
  it "no one else has the base suit or trump suit" do
    let state =
          initialState
            (Card Two Clubs, Player1)
            (Card Jack Hearts, Player2)
            (Card Queen Diamonds, Player3)
            (Card King Hearts, Player4)
    findWinnerOfTrick state `shouldBe` Player1
