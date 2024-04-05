module Test.Socket.MsgSpec where

import Control.Lens (at, ix, preview, traversed, (^..), (^?))
import qualified Data.Map as Map
import Test.Hspec (Spec, around_, before, describe, it, shouldBe, shouldContain, shouldReturn)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Client as WS
import qualified Soltan
import Soltan.App (newChannel)
import Soltan.App.Channel (readChannel, writeChannel)
import Soltan.Socket.Types
import UnliftIO.Async
import UnliftIO.Concurrent

waitSome :: IO ()
waitSome = threadDelay $ 200 * 1000

withSocketServer :: IO r -> IO r
withSocketServer action = do
  serverThread <- async Soltan.runSoltan
  waitSome
  result <- action
  waitSome
  cancel serverThread
  pure result

sendJSON :: WS.Connection -> MsgIn -> IO ()
sendJSON conn = WS.sendTextData conn . BS.toStrict . Aeson.encode @MsgIn

receiveJSON :: WS.Connection -> IO MsgOut
receiveJSON conn = do
  rawMsg <- WS.receiveData conn
  case Aeson.decode @MsgOut (BS.fromStrict rawMsg) of
    Nothing ->
      error $ "parsing error receiving in socket client with data: " <> show rawMsg
    Just msg -> do
      pure msg

spec :: Spec
spec = do
  around_ withSocketServer $ describe "hello" do
    it "should" do
      WS.runClient "127.0.0.1" 9160 "/" $ \conn -> do
        sendJSON conn $ Login "test"
        msg <- receiveJSON conn
        print msg
        msg `shouldBe` Noti "test"

--   describe "getTablesHandler" do
--     it "should return empty table list for empty lobby" do
--       cmds <- runTestApp Map.empty (runExceptT getTablesHandler)
--       cmds `shouldBe` Right [SendMsg (TableList [])]
--     -- it "should return table summary of tables in lobby" do
--     --   lobby <- lobbyFixture
--       -- runTestApp lobby (runExceptT getTablesHandler) `shouldReturn` Right [SendMsg (TableList [TableSummary "A" 0, TableSummary "B" 2, TableSummary "C" 4])]
--
--     describe "subscribeToTableHandler" do
--       it "should not be able to join when table is already full" do
--         lobby <- lobbyFixture
--         Left err <- runTestApp lobby (runExceptT (subscribeToTableHandler "C" "u5"))
--         err `shouldBe` TableIsFull "C"
--       it "should throw error when table does not exist" do
--         lobby <- lobbyFixture
--         Left err <- runTestApp lobby (runExceptT (subscribeToTableHandler "Z" "u5"))
--         err `shouldBe` TableDoesNotExist "Z"
--       it "should be able to join an empty table" do
--         lobby <- lobbyFixture
--         Right cmds <- runTestApp lobby (runExceptT (subscribeToTableHandler "A" "u5"))
--         cmds `shouldBe` [JoinLobby "A" "u5", SendMsg (SuccessfullySubscribedToTable "A" (TableSummary "A" 0))]
--
--

-- Testing channels client

-- void $ async $ WS.runClient "127.0.0.1" 9160 "/" $ \conn -> do
--   let sendThread = do
--         msg <- readChannel sendChannel
--         WS.sendTextData conn (BS.toStrict . Aeson.encode @MsgIn $ msg)
--   let recvThread = do
--         putStrLn "recveiving ==--------------------------------- w"
--         rawMsg <- WS.receiveData conn
--         putStrLn "recveiving ==========------------------------- w"
--         case Aeson.decode @MsgOut (BS.fromStrict rawMsg) of
--           Nothing ->
--             putStrLn "================= shit"
--           Just msg -> do
--             writeChannel recvChannel msg
--   race_ sendThread recvThread
-- putStrLn "test ==--------------------------------- w"
-- writeChannel sendChannel (Login "test")
-- putStrLn "test ========--------------------------- w"
-- msg <- readChannel recvChannel
-- putStrLn "test =================------------------ w"
-- print msg
