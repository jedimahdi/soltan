{-# LANGUAGE RankNTypes #-}
module Soltan.Data.Monoid.Checker
  ( Checker
  , run
  , mk
  , invert
  , when
  , unless
  ) where
import Control.Lens (Getter, Getting)
import Prelude hiding (not, unless, when)

newtype Checker s e = Checker (s -> Either e ())

run :: s -> Checker s e -> Either e ()
run s (Checker p) = p s

instance Semigroup (Checker s e) where
  Checker p <> Checker p' = Checker \s ->
    p s <> p' s

instance Monoid (Checker s e) where
  mempty = Checker \s -> Right ()

mk :: e -> (s -> Bool) -> Checker s e
mk err predicate = Checker \s ->
  if predicate s
    then Right ()
    else Left err

invert :: e -> Checker s e -> Checker s e
invert err (Checker k) = Checker \s ->
  case k s of
    Left _  -> Right ()
    Right _ -> Left err

-- | Check only when condition is true
when :: (s -> Bool) -> Checker s e -> Checker s e
when condition (Checker k) = Checker \s ->
  if condition s
    then k s
    else Right ()

unless :: (s -> Bool) -> Checker s e -> Checker s e
unless condition (Checker k) = Checker \s ->
  if condition s
    then Right ()
    else k s
