module Test.Hokm.Api.Data.Game
    ( tests
    ) where

import           Data.List                   ( (!!) )
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import           Data.UUID.Util              ( UnpackedUUID (..) )
import qualified Data.UUID.Util              as UUID
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Hokm.Api.Data.Card          ( Card )
import qualified Hokm.Api.Data.Card          as Card
import           Hokm.Api.Data.Game          ( Game )
import qualified Hokm.Api.Data.Game          as Game
import qualified Hokm.Api.Data.Text.Username as Username
import qualified Hokm.Api.Data.User          as User
import           Test.Tasty
import           Test.Tasty.Hedgehog

uuid :: MonadGen m => m UUID
uuid = do
  time_low            <- Gen.word32 Range.linearBounded
  time_mid            <- Gen.word16 Range.linearBounded
  time_hi_and_version <- Gen.word16 Range.linearBounded
  clock_seq_hi_res    <- Gen.word8 Range.linearBounded
  clock_seq_low       <- Gen.word8 Range.linearBounded
  node_0              <- Gen.word8 Range.linearBounded
  node_1              <- Gen.word8 Range.linearBounded
  node_2              <- Gen.word8 Range.linearBounded
  node_3              <- Gen.word8 Range.linearBounded
  node_4              <- Gen.word8 Range.linearBounded
  node_5              <- Gen.word8 Range.linearBounded

  pure $ UUID.pack (UnpackedUUID { .. })

username :: MonadGen g => g User.Username
username = Username.Mk <$> Gen.text (Range.linear 8 20) (Gen.choice [Gen.hexit, pure '_'])

genSuit :: MonadGen g => g Card.Suit
genSuit = Gen.enum Card.Club Card.Spade

genValue :: MonadGen g => g Card.Value
genValue = Gen.enum Card.Two Card.Ace

card :: MonadGen g => g Card
card = do
  suit <- genSuit
  value <- genValue
  pure <| Card.Card suit value

genGame :: MonadGen g => Game.Status -> g (Game, User.Username, User.Username)
genGame status = do
  us <- Set.toList <$> Gen.set (Range.singleton 5) username
  let u1 = us !! 0
  let u2 = us !! 1
  let u3 = us !! 2
  let u4 = us !! 3
  let u5 = us !! 3

  cards <- join <| fmap Gen.shuffle <| fmap Set.toList <| Gen.set (Range.singleton 20) card

  suit1 <- genSuit

  gameId <- uuid

  let g = Game.Game { id = gameId
                       , status = status
                       , players = Game.mkPlayers cards us
                       , king = u1
                       , baseSuit = Nothing
                       , turn = Just u1
                       }

  pure (g, u1, u2)

tests :: TestTree
tests =
  testGroup "Test.Cascade.Api.Data.Game" [playCardTests]

playCardTests :: TestTree
playCardTests = testGroup "playCard" [ testProperty "not started yet" prop_notStarted
                                     , testProperty "wrong turn" prop_wrongTurn
                                     , testProperty "update game" prop_update
                                     ]

prop_notStarted :: Property
prop_notStarted = property <| do
  (game, u1, _) <- forAll <| genGame Game.ChooseHokm
  Game.playCard (p1c !! 1) u1 game === Left Game.NotInGame

prop_wrongTurn :: Property
prop_wrongTurn = property <| do
  trumpSuit <- forAll genSuit
  (game, (u1, p1c), (u2, p2c)) <- forAll <| genGame (Game.InGame trumpSuit)
  Game.playCard (p1c !! 1) u2 game === Left Game.WrongTurn

prop_update :: Property
prop_update = property <| do
  trumpSuit <- forAll genSuit
  (game, (u1, p1c), (u2, p2c)) <- forAll <| genGame (Game.InGame trumpSuit)
  (Game.playCard (p1c !! 1) u1 game |> isRight) === True

