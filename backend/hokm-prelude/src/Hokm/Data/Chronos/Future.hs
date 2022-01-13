module Hokm.Data.Chronos.Future
    ( Future
    , mk
    , pattern Future
    , un
    ) where

import           Chronos         ( OffsetDatetime, Time, offsetDatetimeToTime )
import           Control.Lens.TH ( makeWrapped )
import           Data.Aeson      ( FromJSON, ToJSON )

newtype Future a
  = Mk { un :: a }
  deriving stock (Show)
  deriving newtype (Eq, FromJSON, ToJSON)

makeWrapped ''Future

pattern Future :: a -> Future a
pattern Future a <- Mk a
{-# COMPLETE Future #-}

class IsFuture (a :: Type) where
  mk :: a -> Time -> Maybe (Future a)

instance IsFuture OffsetDatetime where
  mk date now = if isPast then Nothing else Just $ Mk date
   where
    isPast :: Bool
    isPast = now > offsetDatetimeToTime date

instance IsFuture Time where
  mk date now = if isPast then Nothing else Just $ Mk date
   where
    isPast :: Bool
    isPast = now > date
