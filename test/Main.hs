module Main (main) where

import qualified Test.Soltan.Game.Action
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests =
  [ Test.Soltan.Game.Action.tests
  ]
