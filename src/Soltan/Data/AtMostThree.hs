{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Soltan.Data.AtMostThree where

import Control.Lens.TH (makePrisms)
import Data.Generics.Labels ()

data AtMostThree a
  = Zero
  | One {first :: a}
  | Two {first :: a, second :: a}
  | Three {first :: a, second :: a, third :: a}
  deriving stock (Show, Eq, Generic, Foldable)
  deriving anyclass (ToJSON)

makePrisms ''AtMostThree
