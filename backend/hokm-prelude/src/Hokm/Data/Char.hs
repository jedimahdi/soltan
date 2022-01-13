module Hokm.Data.Char
    ( module Data.Char
    , isAlphaNumUnderscore
    ) where

import           Data.Char

isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c = isAlphaNum c || c == '_'
