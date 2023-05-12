module Soltan.Game.Action
  ( validateAction
  , gameReducer
  , Error(..)
  ) where

import Control.Lens (ix, (%~), (.~))
import qualified Soltan.Data.Monoid.Checker as Checker
import Soltan.Data.Game (Game, Hokm (..))
import Soltan.Data.Game.Action
import Soltan.Data.Game.Card (Card, Suit)
import qualified Soltan.Data.Monoid.Modifier as Modifier
import Soltan.Data.Username (Username)
import Soltan.Game.Action.Checks
import Soltan.Game.Action.Modifiers

validateAction :: Action 'Unknown -> Game -> Either Error (Action 'Valid)
validateAction action game =
  case action of
    PlayCard username card   -> playCardChecks username card
    ChooseHokm username suit -> chooseHokmChecks username suit
    NextRound                -> nextRoundChecks
  |> fold
  |> Checker.run game
  |> fmap (const (unsafeStatusCoerce action))

playCardChecks :: Username -> Card -> [Checker]
playCardChecks username card
  = [ isHokmChoosen
    , isRightTurn username
    , haveCard username card
    , isRightSuitToPlay username card
    ]

chooseHokmChecks :: Username -> Suit -> [Checker]
chooseHokmChecks username suit = []

nextRoundChecks :: [Checker]
nextRoundChecks = []

gameReducer :: Action 'Valid -> Game -> Game
gameReducer action game =
  case unsafeStatusCoerce @_ @'Unknown action of
    PlayCard username card   -> playCardMods username card
    ChooseHokm username card -> chooseHokmMods username card
    NextRound                -> nextRoundMods
  |> fold
  |> Modifier.run game

nextRoundMods :: [Modifier]
nextRoundMods
  = []

chooseHokmMods :: Username -> Suit -> [Modifier]
chooseHokmMods username suit
  = [ chooseHokm suit
    ]

playCardMods :: Username -> Card -> [Modifier]
playCardMods username card
  = [ removeCardFromHand username card
    , nextTurn
    , addCardToMiddle username card
    ]
