module Effect.Api exposing (..)

import Data.Authentication as Auth exposing (Credential)
import Data.Card as Card exposing (Card)
import Http
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Encode as Encode


decodeErrors : Http.Error -> List String
decodeErrors error =
    case error of
        -- Http.BadStatus response ->
        --     response.body
        --         |> decodeString (field "errors" errorsDecoder)
        --         |> Result.withDefault [ "Server error" ]
        err ->
            [ "Server error" ]


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


findGame : Auth.Credential -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
findGame cred toMsg decoder =
    Http.request
        { method = "GET"
        , headers = [ Auth.header cred ]
        , url = "http://localhost:5000/api/game/find"
        , expect = Http.expectJson toMsg decoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


login : String -> String -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
login username password toMsg decoder =
    Http.post
        { url = "http://localhost:5000/api/authentication/login"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string username )
                    , ( "password", Encode.string password )
                    ]
        , expect = Http.expectJson toMsg decoder
        }


chooseHokm : Auth.Credential -> String -> Card.Suit -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
chooseHokm cred gameId trumpSuit toMsg decoder =
    Http.request
        { method = "POST"
        , url = "http://localhost:5000/api/game/choose-hokm"
        , headers = [ Auth.header cred ]
        , body = Http.jsonBody <| Encode.object [ ( "gameId", Encode.string gameId ), ( "suit", Card.encodeSuit trumpSuit ) ]
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


playCard : Auth.Credential -> String -> Card -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
playCard cred gameId card toMsg decoder =
    Http.request
        { method = "POST"
        , url = "http://localhost:5000/api/game/play"
        , headers = [ Auth.header cred ]
        , body = Http.jsonBody <| Encode.object [ ( "gameId", Encode.string gameId ), ( "card", Card.encode card ) ]
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


endRound : Auth.Credential -> String -> (Result Http.Error a -> msg) -> Decode.Decoder a -> Cmd msg
endRound cred gameId toMsg decoder =
    Http.request
        { method = "POST"
        , url = "http://localhost:5000/api/game/end-round"
        , headers = [ Auth.header cred ]
        , body = Http.jsonBody <| Encode.object [ ( "gameId", Encode.string gameId ) ]
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }
