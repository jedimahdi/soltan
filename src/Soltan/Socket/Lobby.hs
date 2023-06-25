module Soltan.Socket.Lobby where

import Control.Lens (ito, itraversed, lengthOf, to, traversed, (^..), (^@..))
import Pipes.Concurrent (Input, Output, newest, spawn)
import Soltan.Data.Username (Username)
import Soltan.Hokm (initialGame, Game)
import Soltan.Socket.Types (Lobby, Table (..), TableName, TableSummary (..))

initialLobby :: MonadIO m => m Lobby
initialLobby = do
  (output, input) <- liftIO <| spawn <| newest 1
  let table =
        Table
          { subscribers = []
          , gameInMailbox = output
          , gameOutMailbox = input
          , game = initialGame
          }
  pure <| fromList [("Black", table)]

mkTable :: Input Game -> Output Game -> [Username] -> Table
mkTable input output users =
  Table
    { subscribers = users
    , gameInMailbox = output
    , gameOutMailbox = input
    , game = initialGame
    }

summariseTable :: TableName -> Table -> TableSummary
summariseTable tableName table = TableSummary{tableName = tableName, playerCount = lengthOf (#subscribers . traverse) table}

summariseTables :: Lobby -> [TableSummary]
summariseTables lobby = (lobby ^@.. itraversed) |> fmap (uncurry summariseTable)
