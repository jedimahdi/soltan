{-# LANGUAGE DeriveTraversable #-}

module Soltan.Data.Four where

import           Data.Generics.Labels ()

data Four a = Four
  { first  :: a
  , second :: a
  , thrid  :: a
  , fourth :: a
  }
  deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (ToJSON)

mkFromList :: [a] -> Maybe (Four a)
mkFromList [x1, x2, x3, x4] = Just <| Four x1 x2 x3 x4
mkFromList _                = Nothing

toNonEmpty :: Four a -> NonEmpty a
toNonEmpty (Four x1 x2 x3 x4) = x1 :| [x2, x3, x4]
