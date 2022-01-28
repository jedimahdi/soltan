module Data.Game exposing (..)

import Data.Card as Card exposing (Card)
import Dict exposing (Dict)
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import List
import Tuple


type alias Player =
    { username : String
    , playedCard : Maybe Card
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required "username" Decode.string
        |> required "playedCard" (Decode.maybe Card.decoder)


type alias NotFullInternal =
    { id : String
    , joinedPlayers : List String
    }


notFullDecoder : Decoder NotFullInternal
notFullDecoder =
    Decode.succeed NotFullInternal
        |> required "id" string
        |> required "joinedPlayers" (Decode.list string)


type alias ChooseHokmInternal =
    { id : String
    , players : List Player
    , king : String
    , cards : List Card
    }


chooseHokmDecoder : Decoder ChooseHokmInternal
chooseHokmDecoder =
    Decode.succeed ChooseHokmInternal
        |> required "id" string
        |> required "players" (Decode.list playerDecoder)
        |> required "king" Decode.string
        |> required "cards" (Decode.list Card.decoder)


type alias StartedInternal =
    { id : String
    , players : List Player
    , king : String
    , trumpSuit : Card.Suit
    , baseSuit : Maybe Card.Suit
    , cards : List Card
    , turn : Maybe String
    }


middleCardsDecoder : Decoder ( String, Card )
middleCardsDecoder =
    Decode.map2 Tuple.pair Decode.string Card.decoder


startedDecoder : Decoder StartedInternal
startedDecoder =
    Decode.succeed StartedInternal
        |> required "id" string
        |> required "players" (Decode.list playerDecoder)
        |> required "king" Decode.string
        |> required "trumpSuit" Card.suitDecoder
        |> required "baseSuit" (Decode.maybe Card.suitDecoder)
        |> required "cards" (Decode.list Card.decoder)
        |> required "turn" (Decode.maybe Decode.string)


type Game
    = NotFull NotFullInternal
    | ChooseHokm ChooseHokmInternal
    | Started StartedInternal


decoder : Decoder Game
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen decoderHelp


decoderHelp : String -> Decoder Game
decoderHelp tag =
    case tag of
        "NotFull" ->
            Decode.map NotFull notFullDecoder

        "ChooseHokm" ->
            Decode.map ChooseHokm chooseHokmDecoder

        "Started" ->
            Decode.map Started startedDecoder

        _ ->
            Decode.fail "game parse error"


id : Game -> String
id game =
    case game of
        NotFull g ->
            g.id

        ChooseHokm g ->
            g.id

        Started g ->
            g.id
