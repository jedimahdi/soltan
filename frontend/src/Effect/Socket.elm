port module Effect.Socket exposing (..)

import Debug
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


port connectSocket : String -> Cmd msg


port connectSocketWith : ( String, String ) -> Cmd msg


port onConnect : (String -> msg) -> Sub msg


port onMessages : (String -> msg) -> Sub msg


onConnectFrom : String -> msg -> Sub (Maybe msg)
onConnectFrom url msg =
    onConnect
        (\u ->
            if u == url then
                Just msg

            else
                Nothing
        )


type alias SocketData =
    { url : String
    , data : String
    }


dataDecoder : Decode.Decoder SocketData
dataDecoder =
    Decode.succeed SocketData
        |> required "url" Decode.string
        |> required "data" Decode.string


onMessagesFrom : String -> Decode.Decoder a -> (a -> msg) -> Sub (Maybe msg)
onMessagesFrom url decoder f =
    let
        handleMessage s =
            Debug.log s <|
                case Decode.decodeString dataDecoder s of
                    Err _ ->
                        Debug.log "wtf not decode" Nothing

                    Ok data ->
                        if data.url == url then
                            case Decode.decodeString decoder data.data of
                                Err _ ->
                                    Debug.log "noooo" Nothing

                                Ok a ->
                                    Debug.log "yess" (Just <| f a)

                        else
                            Nothing
    in
    onMessages handleMessage
