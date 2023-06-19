module Test.Socket.MsgSpec where

import Control.Lens (at, ix, preview, traversed, (^..), (^?))
import qualified Data.Map as Map
import Soltan.Effects.Lobby (AcquireLobby (..))
import Soltan.Effects.LogMessages (LogMessages (..))
import Soltan.Effects.Now (Now)
import Soltan.Effects.Random (MonadRandom)
import Soltan.Socket.Msg
import Soltan.Socket.Types
import Test.Hspec (Spec, describe, it, shouldBe)
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

spec :: Spec
spec = describe "getTablesHandler" do
  it "should return empty table list for empty lobby" do
    cmds <- runTestApp Map.empty getTablesHandler
    cmds `shouldBe` [SendMsg (TableList [])]
