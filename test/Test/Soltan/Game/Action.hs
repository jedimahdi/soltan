{-# LANGUAGE TupleSections #-}
module Test.Soltan.Game.Action where

import Control.Lens (ix, to)
import Data.List ((!!))
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Soltan.Data.Game as Game
import Soltan.Data.Game
import Soltan.Data.Game.Action
import Soltan.Data.Game.Card
import qualified Soltan.Data.Username as Username
import Soltan.Data.Username (Username)
import Soltan.Game.Action
import System.Random (getStdGen)
import System.Random.Shuffle (shuffle')
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

-- throwLeft :: HasCallStack => Either e a -> a
-- throwLeft (Right a) = a
-- throwLeft (Left e)  = error "throwLeft"
--
-- mkGame :: [Text] -> [[Card]] -> Game.Hokm -> IO Game
-- mkGame users cards hokm = either (const (fail "mkGame error")) pure game
--   where
--     game = do
--       usernames <- traverse Username.mk users |> first (const WrongUsers)
--       let hands = Map.fromList <| zip usernames cards
--       Game.mk usernames hands [] (usernames !! 1) hokm
--
-- startingGame :: IO Game
-- startingGame = do
--   let usernames = ["user1", "user2", "user3", "user4"] |> fmap Username.mk |> fmap throwLeft
--   gen <- getStdGen
--   let hands = shuffle' fullDeck (length fullDeck) gen |> chunksOf 13 |> zip usernames |> Map.fromList
--   pure <| throwLeft <| Game.mk usernames hands [] (usernames !! 0) (Game.Choosed Spade)

tests :: TestTree
tests = testGroup "Soltan.Game.Action" []

-- canPerformActionTests :: TestTree
-- canPerformActionTests = testGroup "canPerformAction" [playCardTests]

-- playCardTests :: TestTree
-- playCardTests = testGroup "PlayCard Action"
--   [ testCase "hookm is not choosen" hokmNotChoosen
--   , testCase "should fail with wrong turn" wrongTurn
--   , testCase "should success in starting game with hakem playing his hand" success
--   ]
--   where
--     hokmNotChoosen = do
--       game <- mkGame ["user1", "user2", "user3", "user4"] [] Game.NotChoosed
--       let usernames = game ^. #players . to toList
--       let action = PlayCard (usernames !! 2) (Card Spade Ace)
--       canPerformAction action game @?= Left HokmNotChoosen
--     wrongTurn = do
--       game <- startingGame
--       let usernames = game ^. #players . to toList
--       let action = PlayCard (usernames !! 1) ((game ^. #hands . ix (game ^. #hakem)) !! 0)
--       canPerformAction action game @?= Left WrongTurn
--     success = do
--       game <- startingGame
--       let usernames = game ^. #players . to toList
--       let action = PlayCard (game ^. #hakem) ((game ^. #hands . ix (game ^. #hakem)) !! 0)
--       assertBool "should be Right" <| isRight <| canPerformAction action game
--
