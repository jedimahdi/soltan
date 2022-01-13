module Data.Game exposing (..)

import Data.Card as Card exposing (Card)
import Dict exposing (Dict)
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import List
import Tuple


type alias Hands =
    Dict String (List Card)


handsDecoder : Decoder Hands
handsDecoder =
    Decode.dict (Decode.list Card.decoder)


type alias Player =
    { idx : Int
    , username : String
    , team : String
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required "idx" Decode.int
        |> required "username" Decode.string
        |> required "team" Decode.string


type alias NotFullInternal =
    { id : String
    , joinedPlayers : List String
    , fullCount : Int
    }


notFullDecoder : Decoder NotFullInternal
notFullDecoder =
    Decode.succeed NotFullInternal
        |> required "id" string
        |> required "joinedPlayers" (Decode.list string)
        |> required "fullCount" Decode.int


type alias ChooseHokmInternal =
    { id : String
    , players : List Player
    , king : Int
    , hands : Hands
    , fullCount : Int
    }


chooseHokmDecoder : Decoder ChooseHokmInternal
chooseHokmDecoder =
    Decode.succeed ChooseHokmInternal
        |> required "id" string
        |> required "players" (Decode.list playerDecoder)
        |> required "king" Decode.int
        |> required "hands" handsDecoder
        |> required "fullCount" Decode.int


type alias StartedInternal =
    { id : String
    , players : List Player
    , king : Int
    , trumpSuit : Card.Suit
    , baseSuit : Maybe Card.Suit
    , hands : Hands
    , turn : Maybe Int
    , middleCards : List ( String, Card )
    , fullCount : Int
    }


middleCardsDecoder : Decoder ( String, Card )
middleCardsDecoder =
    Decode.map2 Tuple.pair Decode.string Card.decoder


startedDecoder : Decoder StartedInternal
startedDecoder =
    Decode.succeed StartedInternal
        |> required "id" string
        |> required "players" (Decode.list playerDecoder)
        |> required "king" Decode.int
        |> required "trumpSuit" Card.suitDecoder
        |> required "baseSuit" (Decode.maybe Card.suitDecoder)
        |> required "hands" handsDecoder
        |> required "turn" (Decode.maybe Decode.int)
        |> required "middleCards" (Decode.list middleCardsDecoder)
        |> required "fullCount" Decode.int


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


king : ChooseHokmInternal -> Maybe String
king game =
    List.head <| List.map (\p -> p.username) <| List.filter (\p -> p.idx == game.king) game.players
