module Test.Socket.MsgSpec where

import Control.Lens (at, ix, preview, traversed, (^..), (^?))
import qualified Data.Map as Map
import Pipes.Concurrent (newest, spawn)
import Soltan.Effects.Lobby (AcquireLobby (..))
import Soltan.Effects.LogMessages (LogMessages (..))
import Soltan.Effects.Now (Now)
import Soltan.Effects.Random (MonadRandom)
import Soltan.Hokm.Types (Game (..), PlayerIndex (..))
import Soltan.Hokm.Utils (mkChooseHokmState)
import Soltan.Socket.Lobby (mkTable)
import Soltan.Socket.Msg
import Soltan.Socket.Types
import System.Random (getStdGen)
import Test.Hspec (Spec, before, describe, it, shouldBe, shouldContain)
import UnliftIO (MonadUnliftIO)

newtype TestApp a = TestApp {runApp :: ReaderT Lobby IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Lobby, Now, MonadRandom)

instance AcquireLobby TestApp where
  getTable tableName = asks <| preview (ix tableName)
  getLobby = ask

instance LogMessages TestApp where
  logMessage _ = pass

runTestApp :: Lobby -> TestApp a -> IO a
runTestApp lobby app = usingReaderT lobby <| runApp app

lobbyFixture :: IO Lobby
lobbyFixture = do
  (output, input) <- spawn <| newest 1
  (output2, input2) <- spawn <| newest 1
  (output3, input3) <- spawn <| newest 1
  gen <- getStdGen
  pure
    <| Map.fromList
      [ ("A", Table{subscribers = [], gameOutMailbox = input, gameInMailbox = output, game = GameBeforeStart})
      , ("B", Table{subscribers = ["u1", "u2"], gameOutMailbox = input2, gameInMailbox = output2, game = GameBeforeStart})
      ,
        ( "C"
        , Table
            { subscribers = ["u1", "u2", "u3", "u4"]
            , gameOutMailbox = input2
            , gameInMailbox = output2
            , game = GameChoosingHokm <| mkChooseHokmState gen Player1 0 0 "u1" "u2" "u3" "u4"
            }
        )
      ]

spec :: Spec
spec = do
  describe "getTablesHandler" do
    it "should return empty table list for empty lobby" do
      cmds <- runTestApp Map.empty (runExceptT getTablesHandler)
      cmds `shouldBe` Right [SendMsg (TableList [])]
    it "should return table summary of tables in lobby" do
      lobby <- lobbyFixture
      Right cmds <- runTestApp lobby (runExceptT getTablesHandler)
      cmds `shouldBe` [SendMsg (TableList [TableSummary "A" 0, TableSummary "B" 2, TableSummary "C" 4])]

    describe "subscribeToTableHandler" do
      it "should not be able to join when table is already full" do
        lobby <- lobbyFixture
        Left err <- runTestApp lobby (runExceptT (subscribeToTableHandler "C" "u5"))
        err `shouldBe` TableIsFull "C"
      it "should throw error when table does not exist" do
        lobby <- lobbyFixture
        Left err <- runTestApp lobby (runExceptT (subscribeToTableHandler "Z" "u5"))
        err `shouldBe` TableDoesNotExist "Z"
      it "should be able to join an empty table" do
        lobby <- lobbyFixture
        Right cmds <- runTestApp lobby (runExceptT (subscribeToTableHandler "A" "u5"))
        cmds `shouldBe` [JoinLobby "A" "u5", SendMsg (SuccessfullySubscribedToTable "A" GameBeforeStart)]
