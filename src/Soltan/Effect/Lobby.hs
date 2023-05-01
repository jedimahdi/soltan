{-# LANGUAGE KindSignatures           #-}
module Soltan.Effect.Lobby where

import           Control.Lens         (at, to, (%~))
import           Data.Constraint      (Dict (..))
import qualified Data.Map             as Map
import           Soltan.App.Env
import           Soltan.App.Lobby
import           Soltan.App.Monad     (App)
import qualified Soltan.Data.Game     as Game
import           Soltan.Data.Username (Username)
import           Unsafe.Coerce        (unsafeCoerce)
import qualified Soltan.Effect.Hub as Hub
import Soltan.Effect.Hub (MonadHub)

class Monad m => MonadLobby m where
  withRoom :: Game.Id -> (Room 'Open -> m ()) -> (Room 'Full -> m ()) -> m ()
  joinRoom :: Username -> Room 'Open -> m (Either (Room 'Open) (Room 'Full))
  leaveRoom :: Username -> Room 'Open -> m (Room 'Open)
  addRoom :: Room 'Open -> m ()
  getRooms :: m [SomeRoom]

instance MonadLobby App where
  withRoom = withRoomImpl
  joinRoom = joinRoomImpl
  leaveRoom = leaveRoomImpl
  addRoom = addRoomImpl
  getRooms = getRoomsImpl

broadcastRoom :: forall m a t. (ToJSON a, MonadLobby m, MonadHub m, Foldable t) => (Username -> t a) -> Game.Id -> m ()
broadcastRoom mkMsg gameId = withRoom gameId broadcast broadcast
  where
    broadcast :: Room s -> m ()
    broadcast room = traverse_ (\username -> Hub.sendJSON (mkMsg username) username) (room ^. #users)

broadcastRoom' :: (ToJSON a, MonadHub m, MonadLobby m) => a -> Game.Id -> m ()
broadcastRoom' msg = broadcastRoom (const . Just <| msg) 

type WithLobby r m = (MonadReader r m, Has Lobby r, MonadIO m)

unsafeStatusCoerce :: Room s -> Room s'
unsafeStatusCoerce = coerce

withRoomImpl :: forall r m. WithLobby r m => Game.Id -> (Room 'Open -> m ()) -> (Room 'Full -> m ()) -> m ()
withRoomImpl gameId onOpen onFull = do
  lobbyVar <- grab @Lobby
  lobby <- readTVarIO lobbyVar
  lobby ^. at gameId |> maybe pass go
  where
    go :: SomeRoom -> m ()
    go someRoom =
      withSomeRoom someRoom \room -> do
        case room ^. #status of
          Open -> onOpen (unsafeStatusCoerce room)
          Full -> onFull (unsafeStatusCoerce room)

joinRoomImpl :: WithLobby r m => Username -> Room 'Open -> m (Either (Room 'Open) (Room 'Full))
joinRoomImpl username room = do
  let room' = room |> #users %~ (username :)
  let result = if room' ^. #users . to length >= 4
            then Right <| unsafeStatusCoerce room'
            else Left room'
  let roomId = room ^. #id
  lobbyVar <- grab @Lobby
  atomically <| modifyTVar' lobbyVar (Map.insert roomId (SomeRoom room'))
  pure result

leaveRoomImpl :: WithLobby r m => Username -> Room 'Open -> m (Room 'Open)
leaveRoomImpl username room = do
  let room' = room |> #users %~ filter (/= username)
  let roomId = room ^. #id
  lobbyVar <- grab @Lobby
  atomically <| modifyTVar' lobbyVar (Map.insert roomId (SomeRoom room'))
  pure room'

addRoomImpl :: WithLobby r m => Room 'Open -> m ()
addRoomImpl room = do
  let roomId = room ^. #id
  lobbyVar <- grab @Lobby
  atomically <| modifyTVar' lobbyVar (Map.insert roomId (SomeRoom room))

getRoomsImpl :: WithLobby r m => m [SomeRoom]
getRoomsImpl = do
  lobbyVar <- grab @Lobby
  lobbyMap <- readTVarIO lobbyVar
  pure <| Map.elems lobbyMap

