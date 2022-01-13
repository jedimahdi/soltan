module Data.Authentication exposing (..)

import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type Credential
    = Credential String String


username : Credential -> String
username (Credential val _) =
    val


header : Credential -> Http.Header
header (Credential _ str) =
    Http.header "Authorization" ("Bearer " ++ str)


decoder : Decoder Credential
decoder =
    Decode.succeed Credential
        |> required "username" Decode.string
        |> required "token" Decode.string


encode : Credential -> Encode.Value
encode (Credential name token) =
    Encode.object
        [ ( "username", Encode.string name )
        , ( "token", Encode.string token )
        ]
