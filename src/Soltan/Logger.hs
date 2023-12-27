module Soltan.Logger where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Soltan.Logger.Message

startLoggerThread :: TChan LogMessage -> IO ()
startLoggerThread logChan = void $ forkIO $ forever do
  msg <- atomically <| readTChan logChan
  putTextLn <| prettyPrintLogMessage msg

logMsg :: TChan LogMessage -> LogMessage -> IO ()
logMsg chan = atomically . writeTChan chan
