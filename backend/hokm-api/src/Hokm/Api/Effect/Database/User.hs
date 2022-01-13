module Hokm.Api.Effect.Database.User
    ( UserL
    , create
    , findById
    , findByUsername
    , getAll
    , run
    ) where

import           Control.Lens                      ( (^.) )
import           Database.PostgreSQL.Simple        ( FromRow, Only (Only) )
import qualified Database.PostgreSQL.Simple        as Database
import           Hokm.Api.Data.ByteString.Password ( Password )
import           Hokm.Api.Data.Text.Username       ( Username )
import qualified Hokm.Api.Data.User                as User
import           Hokm.Api.Effect.Scrypt            ( ScryptL )
import qualified Hokm.Api.Effect.Scrypt            as Scrypt
import qualified Hokm.Data.Validation              as Validation
import           Polysemy                          ( Embed, Members, Sem, interpret, makeSem )
import           Polysemy.Reader                   ( Reader, ask )
import           Prelude                           hiding ( Reader, ask, getAll )

data UserL m a where
  FindByUsername :: User.Username -> UserL m (Maybe User.Table)
  FindById :: User.Id -> UserL m (Maybe User.Readable)
  Create :: User.Creatable 'Validation.Parsed -> UserL m (Maybe User.Readable)
  GetAll :: UserL m [User.Readable]

makeSem ''UserL

run :: Members '[ScryptL, Reader Database.Connection, Embed IO] r => Sem (UserL ': r) a -> Sem r a
run = interpret \case
  FindByUsername username -> do
    conn <- ask
    users <- liftIO <| selectByUsername conn username
    pure <| viaNonEmpty head users

  FindById id -> do
    conn <- ask
    users <- liftIO <| selectById conn id
    pure <| viaNonEmpty head users

  Create creatable -> do
    conn <- ask
    encryptedPassword <- creatable ^. #password |> Scrypt.encryptPassword |> fmap Scrypt.un
    users <- liftIO <| insertUser conn creatable encryptedPassword
    pure <| viaNonEmpty head users

  GetAll -> do
    conn <- ask
    liftIO <| selectAll conn


selectAll :: Database.Connection -> IO [User.Readable]
selectAll conn = Database.query_ conn "select id,username,email_address from users"

selectByUsername :: Database.Connection -> Username -> IO [User.Table]
selectByUsername conn username = Database.query conn "select id,username,email_address,encrypted_password from users where username = ?" (Only username)

selectById :: Database.Connection -> User.Id -> IO [User.Readable]
selectById conn id = Database.query conn "select id,username,email_address from users where id = ?" (Only id)

insertUser :: Database.Connection -> User.Creatable 'Validation.Parsed -> ByteString -> IO [User.Readable]
insertUser conn user password = Database.query conn "insert into users (username, email_address, encrypted_password) values (?,?,?) returning id,username,email_address"  (user ^. #username, user ^. #emailAddress, password)
