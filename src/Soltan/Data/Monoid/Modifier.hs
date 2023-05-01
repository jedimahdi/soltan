module Soltan.Data.Monoid.Modifier
    ( Modifier
    , mk
    , run
    ) where

type Modifier a = Endo a

run :: a -> Modifier a -> a
run = flip appEndo

mk :: (a -> a) -> Modifier a
mk = Endo
