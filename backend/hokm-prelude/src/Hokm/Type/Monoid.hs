module Hokm.Type.Monoid
    ( Concat
    , type (<>)
    ) where

infixr 6 <>
type a <> b = Concat a b
type family Concat (a :: k) (b :: k) :: k

type instance Concat ('[] :: [k]) (lst :: [k]) = lst
type instance Concat ((l ': ls) :: [k]) (lst :: [k]) = l ': Concat ls lst
