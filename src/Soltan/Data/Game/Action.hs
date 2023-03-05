{-# LANGUAGE StandaloneDeriving #-}
module Soltan.Data.Game.Action
  ( Action(..)
  , ActionStatus(..)
  , Error(..)
  , unsafeStatusCoerce
  ) where

import Unsafe.Coerce (unsafeCoerce)
import Soltan.Data.Game.Card (Card, Suit)
import Soltan.Data.Username (Username)

data ActionStatus = Unknown | Valid | Invalid

data Action status where
 PlayCard   :: Username -> Card -> Action 'Unknown
 ChooseHokm :: Username -> Suit -> Action 'Unknown
 NextRound  :: Action 'Unknown

deriving instance Eq (Action s)
deriving instance Show (Action s)

data Error
  = WrongTurn
  | AlreadyStarted
  | NotInGame
  | RoundNotEnded
  | UsernameNotFound
  | EndOfRound
  | WrongSuit
  | GameNotFound
  | HokmNotChoosen
  | HokmIsChoosen
  | CardNotFound
  deriving stock (Show, Eq)

unsafeStatusCoerce :: Action s -> Action s'
unsafeStatusCoerce = unsafeCoerce
