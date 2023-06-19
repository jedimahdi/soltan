module Soltan.Socket.Lobby where

import Control.Lens (ito, itraversed, lengthOf, to, traversed, (^..), (^@..))
import Pipes.Concurrent (Input, Output, newest, spawn)
import Soltan.Data.Username (Username)
import Soltan.Hokm.Types (Game (..))
import Soltan.Socket.Types (Lobby, Table (..), TableName, TableSummary (..))

initialLobby :: MonadIO m => m Lobby
initialLobby = do
  -- randGen <- getStdGen
  -- let shuffledDeck' = shuffledDeck randGen
  (output, input) <- liftIO <| spawn <| newest 1
  let table =
        Table
          { subscribers = []
          , gameInMailbox = output
          , gameOutMailbox = input
          , game = GameBeforeStart
          }
  pure <| fromList [("Black", table)]

mkTable :: Input Game -> Output Game -> [Username] -> Table
mkTable input output users =
  Table
    { subscribers = []
    , gameInMailbox = output
    , gameOutMailbox = input
    , game = GameBeforeStart
    }

summariseTable :: TableName -> Table -> TableSummary
summariseTable tableName table = TableSummary{tableName = tableName, playerCount = lengthOf (#subscribers . traverse) table}

summariseTables :: Lobby -> [TableSummary]
summariseTables lobby = (lobby ^@.. itraversed) |> fmap (uncurry summariseTable)
