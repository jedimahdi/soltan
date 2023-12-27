module Soltan.Socket.Utils where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS

receiveJSON :: (FromJSON a) => WS.Connection -> IO (Maybe a)
receiveJSON conn = do
  m <- WS.receiveData conn
  pure <| Aeson.decode . BS.fromStrict <| m
