module Soltan.Socket.Utils where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Soltan.Socket.Types (Lobby (..), MsgIn, MsgOut, Table, TableName)

encodeMsgToJSON :: MsgOut -> Text
encodeMsgToJSON a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

unLobby :: Lobby -> Map TableName Table
unLobby (Lobby lobby) = lobby
