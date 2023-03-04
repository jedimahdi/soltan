{-# LANGUAGE TupleSections #-}
module Test.Soltan.Game.Action where

import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Soltan.Game.Action
import Soltan.Data.Game
import qualified Soltan.Data.Game as Game
import Soltan.Data.Game.Action
import Soltan.Data.Game.Card
import Soltan.Data.Username (Username)
import qualified Soltan.Data.Username as Username
import qualified Data.Map as Map
import Control.Lens (ix)
import Data.List ((!!))

mkGame :: [Text] -> [[Card]] -> Game.Hokm -> IO (Game, [Username])
mkGame users cards hokm = do
  let game = 
        do
          usernames <- traverse Username.mk users |> first (const WrongUsers)
          let hands = Map.fromList <| zip usernames cards
          Game.mk usernames hands [] (usernames !! 1) hokm |> fmap (, usernames)
  case game of 
    Left _ -> fail "mkGame error"
    Right g -> pure g

tests :: TestTree
tests = testGroup "Soltan.Game.Action" [canPerformActionTests]

canPerformActionTests :: TestTree
canPerformActionTests = testGroup "canPerformAction" [playCardTests]

playCardTests :: TestTree
playCardTests = testGroup "PlayCard Action" [testCase "hookm is not choosen" hokmNotChoosen]
  where
    hokmNotChoosen = do
      (game, usernames) <- mkGame ["user1", "user2", "user3", "user4"] [] Game.NotChoosed
      assertBool "boooool" (isLeft (canPerformAction (PlayCard (usernames !! 2) (Card Spade Ace)) game)) 
