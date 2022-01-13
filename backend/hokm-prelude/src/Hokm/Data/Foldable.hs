module Hokm.Data.Foldable
    ( defaulting
    ) where

defaulting :: Foldable t => b -> (t a -> b) -> t a -> b
defaulting def f ta | null ta   = def
                    | otherwise = f ta
