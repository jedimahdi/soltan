module Hokm.Api.Effect.Scrypt
    ( Encrypted
    , ScryptL
    , encryptPassword
    , pattern Encrypted
    , run
    , un
    , verifyPassword
    ) where

import           Control.Lens.TH                   ( makeWrapped )
import qualified Crypto.Scrypt                     as Scrypt
import           Hokm.Api.Data.ByteString.Password ( Password, pattern Password )
import qualified Hokm.Api.Data.ByteString.Password as Password
import           Database.PostgreSQL.Simple.FromField ( FromField )
import           Polysemy                          ( Embed, Member, Sem, embed, interpret, makeSem )

newtype Encrypted (a :: Type)
  = Mk { un :: ByteString }
  deriving newtype (Eq, Ord, Show, FromField)

makeWrapped ''Encrypted

pattern Encrypted :: ByteString -> Encrypted a
pattern Encrypted a <- Mk a
{-# COMPLETE Encrypted #-}

data ScryptL (m :: Type -> Type) (a :: Type) where Encrypt :: ByteString -> ScryptL m (Encrypted b)

makeSem ''ScryptL

encryptPassword :: Member ScryptL r => Password -> Sem r (Encrypted Password)
encryptPassword = encrypt . Password.un

verifyPassword :: Password -> Encrypted Password -> Bool
verifyPassword (Password p) ep = Scrypt.verifyPass' (coerce p) (coerce ep)

run :: Member (Embed IO) r => Sem (ScryptL ': r) a -> Sem r a
run = interpret \case
  Encrypt x -> Scrypt.encryptPassIO' (coerce x) |> coerce |> fmap Mk |> embed
