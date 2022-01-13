port module Effect.Store exposing (..)

import Json.Decode as Decode


port storeCache : Maybe Decode.Value -> Cmd msg


port onStoreChange : (Decode.Value -> msg) -> Sub msg
