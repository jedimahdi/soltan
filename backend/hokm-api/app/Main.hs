module Main where

import qualified Database.PostgreSQL.Simple as Database
import qualified Hokm.Api
import qualified Hokm.Logger                as Logger
import           Hokm.Logger.Message        ( Scope (..) )
import qualified Hokm.Logger.Message        as Logger.Message

connectionInfo :: Database.ConnectInfo
connectionInfo = Database.ConnectInfo { connectHost = "localhost"
                                      , connectPort = 5432
                                      , connectUser = "mahdi"
                                      , connectPassword = ""
                                      , connectDatabase = "hokm"
                                      }

main :: IO ()
main = do
  conn <- Database.connect connectionInfo
  Hokm.Api.main Hokm.Api.Config { port      = 5000
                                , logAction = Logger.Message.Scoped Api >$< Logger.logScopedMessageToStdStreams
                                , dbConnection = conn
                                }
