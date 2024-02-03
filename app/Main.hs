module Main where

import qualified Soltan
import System.IO (hSetEncoding, utf8)

main :: IO ()
main =
  hSetEncoding stdout utf8 >> Soltan.main
