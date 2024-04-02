module Test.Socket.MsgSpec where

import Control.Lens (at, ix, preview, traversed, (^..), (^?))
import qualified Data.Map as Map
import Test.Hspec (Spec, before, describe, it, shouldBe, shouldContain, shouldReturn)

spec :: Spec
spec = do
  pass
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
