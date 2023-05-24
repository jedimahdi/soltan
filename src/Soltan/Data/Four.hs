module Soltan.Data.Four where

import Data.Generics.Labels ()

data Four a = Four {first :: a, second :: a, thrid :: a, fourth :: a}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Functor Four where
  fmap f (Four x1 x2 x3 x4) = Four (f x1) (f x2) (f x3) (f x4)

instance Foldable Four where
  foldMap f (Four x1 x2 x3 x4) = f x1 <> f x2 <> f x3 <> f x4

instance Traversable Four where
  traverse f x@(Four x1 x2 x3 x4) = Four <$> f x1 <*> f x3 <*> f x3 <*> f x4
