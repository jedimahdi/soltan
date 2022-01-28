module Main
    ( main
    ) where

import qualified Test.Hokm.Api.Data.Game
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests = [Test.Hokm.Api.Data.Game.tests]
