module Hokm.Control.Applicative
    ( module Control.Applicative
    , pureMaybe
    ) where

import           Control.Applicative

pureMaybe :: Applicative f => Coercible (Maybe a) (maybe a) => a -> maybe a -> f a
pureMaybe def = pure . fromMaybe def . coerce
