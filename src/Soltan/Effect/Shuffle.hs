module Soltan.Effect.Shuffle where

import Soltan.App.Monad (App)
import System.Random.Shuffle (shuffleM)
import Soltan.Data.Game.Card (Card, Deck, fullDeck)

class MonadShuffle m where
  shuffle :: [a] -> m [a]

instance MonadShuffle App where
  shuffle = shuffleM

mkShuffledFullDeck :: MonadShuffle m => m [Card]
mkShuffledFullDeck = shuffle fullDeck
