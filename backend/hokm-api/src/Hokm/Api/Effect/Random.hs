module Hokm.Api.Effect.Random
    ( RandomL
    , randomUUID
    , makeSuffledDeck
    , run
    ) where

import           Control.Lens       ( (^.) )
import qualified Data.Map           as Map
import           Data.UUID.V4       ( nextRandom )
import           Hokm.Api.Data.Game ( Game )
import           Hokm.Api.Data.Card ( Card )
import qualified Hokm.Api.Data.Game as Game
import qualified Hokm.Api.Data.Card as Card
import           Polysemy           ( Embed, Member, Sem, interpret, makeSem )
import           System.Random         ( mkStdGen )
import           System.Random.Shuffle ( shuffle' )

data RandomL m a where 
  RandomUUID :: RandomL m UUID
  MakeSuffledDeck :: RandomL m [Card]

makeSem ''RandomL

run :: Member (Embed IO) r => Sem (RandomL ': r) a -> Sem r a
run = interpret \case
  RandomUUID -> liftIO nextRandom
  MakeSuffledDeck -> do
    let pureGen = mkStdGen 137
    let cards = shuffle' Card.makeDeck 52 pureGen
    pure cards
