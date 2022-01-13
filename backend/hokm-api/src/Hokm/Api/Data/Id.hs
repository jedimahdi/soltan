module Hokm.Api.Data.Id
    ( Id (..)
    ) where

import           Control.Lens.TH                      ( makeWrapped )
import           Data.Aeson                           ( FromJSON, ToJSON )
import           Database.PostgreSQL.Simple.FromField ( FromField )
import           Database.PostgreSQL.Simple.ToField   ( ToField )
import           Libjwt.Classes                       ( JwtRep )
import           Servant.API                          ( FromHttpApiData, ToHttpApiData )

newtype Id (entity :: Type)
  = Id { unId :: UUID }
  deriving stock (Generic)
  deriving newtype
  ( Eq
  , FromField
  , FromHttpApiData
  , FromJSON
  , JwtRep ByteString
  , Ord
  , Show
  , ToField
  , ToHttpApiData
  , ToJSON
  )

makeWrapped ''Id
