module Effect.Store.Credential exposing (..)

import Browser.Navigation as Nav
import Data.Authentication as Auth exposing (Credential)
import Data.Session exposing (Session)
import Effect.Store exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Encode as Encode


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    viewerChanges (\maybeViewer -> toMsg (Data.Session.mk key maybeViewer))


storageDecoder : Decoder Credential
storageDecoder =
    Decode.field "user" Auth.decoder


viewerChanges : (Maybe Credential -> msg) -> Sub msg
viewerChanges toMsg =
    onStoreChange (\value -> toMsg (decodeFromChange value))


decodeFromChange : Value -> Maybe Credential
decodeFromChange val =
    Decode.decodeValue storageDecoder val
        |> Result.toMaybe


storeCredWith : Credential -> Cmd msg
storeCredWith (Auth.Credential uname token) =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Encode.string uname )
                        , ( "token", Encode.string token )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing
