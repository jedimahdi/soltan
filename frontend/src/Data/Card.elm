module Data.Card exposing (..)

import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode


type Suit
    = Club
    | Diamond
    | Heart
    | Spade


suitDecoder : Decoder Suit
suitDecoder =
    Decode.string |> Decode.andThen suitDecoderHelp


suitToString : Suit -> String
suitToString suit =
    case suit of
        Club ->
            "Club"

        Diamond ->
            "Diamond"

        Heart ->
            "Heart"

        Spade ->
            "Spade"


encodeSuit : Suit -> Encode.Value
encodeSuit suit =
    Encode.string <| suitToString suit


suits : List Suit
suits =
    [ Club, Diamond, Heart, Spade ]


suitDecoderHelp : String -> Decoder Suit
suitDecoderHelp suit =
    case suit of
        "Club" ->
            Decode.succeed Club

        "Diamond" ->
            Decode.succeed Diamond

        "Heart" ->
            Decode.succeed Heart

        "Spade" ->
            Decode.succeed Spade

        _ ->
            Decode.fail "fail to decode card suit"


type Value
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


valueToString : Value -> String
valueToString value =
    case value of
        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"

        Ace ->
            "Ace"


encodeValue : Value -> Encode.Value
encodeValue value =
    Encode.string <| valueToString value


valueDecoder : Decoder Value
valueDecoder =
    Decode.string |> Decode.andThen valueDecoderHelp


valueDecoderHelp : String -> Decoder Value
valueDecoderHelp value =
    case value of
        "Two" ->
            Decode.succeed Two

        "Three" ->
            Decode.succeed Three

        "Four" ->
            Decode.succeed Four

        "Five" ->
            Decode.succeed Five

        "Six" ->
            Decode.succeed Six

        "Seven" ->
            Decode.succeed Seven

        "Eight" ->
            Decode.succeed Eight

        "Nine" ->
            Decode.succeed Nine

        "Ten" ->
            Decode.succeed Ten

        "Jack" ->
            Decode.succeed Jack

        "Queen" ->
            Decode.succeed Queen

        "King" ->
            Decode.succeed King

        "Ace" ->
            Decode.succeed Ace

        _ ->
            Decode.fail "fail to decode card value"


type alias Card =
    { suit : Suit
    , value : Value
    }


encode : Card -> Encode.Value
encode card =
    Encode.object
        [ ( "suit", encodeSuit card.suit )
        , ( "value", encodeValue card.value )
        ]


decoder : Decoder Card
decoder =
    Decode.succeed Card
        |> required "suit" suitDecoder
        |> required "value" valueDecoder
