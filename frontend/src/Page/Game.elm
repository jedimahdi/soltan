module Page.Game exposing (..)

import Data.Authentication as Auth exposing (Credential)
import Data.Card as Card exposing (Card)
import Data.Game as Game exposing (Game)
import Data.Session as Session exposing (..)
import Dict
import Effect.Api as Api
import Effect.Socket as Socket
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Encode as Encode
import List
import Maybe
import Page exposing (Page)
import String
import View.Card


type alias Model =
    { session : Session
    , gameId : String
    , game : Maybe Game
    }


type Msg
    = GotSession Session
    | GotGame Game
    | OnConnect
    | KingChooseHokm Card.Suit
    | KingChooseHokmRequestCompleted (Result Http.Error Game)
    | PlayCard Card
    | PlayCardRequestCompleted (Result Http.Error Game)
    | NoOp


maybeMsg : Maybe Msg -> Msg
maybeMsg msg =
    Maybe.withDefault NoOp msg


socketUrl : String -> String
socketUrl gameId =
    "ws://localhost:5000/socket/game/" ++ gameId


init : Session -> String -> ( Model, Cmd Msg )
init session gameId =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , gameId = gameId
      , game = Nothing
      }
    , case maybeCred of
        Nothing ->
            Cmd.none

        Just cred ->
            Socket.connectSocketWith <| ( socketUrl gameId, Encode.encode 0 (Auth.encode cred) )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.session of
        Guest _ ->
            ( model, Cmd.none )

        LoggedIn _ cred ->
            updateWithAuth cred msg model


updateWithAuth : Auth.Credential -> Msg -> Model -> ( Model, Cmd Msg )
updateWithAuth cred msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        GotGame game ->
            ( { model | game = Just game }, Cmd.none )

        KingChooseHokm trumpSuit ->
            ( model
            , Api.chooseHokm cred model.gameId trumpSuit KingChooseHokmRequestCompleted Game.decoder
            )

        PlayCard card ->
            ( model
            , Api.playCard cred model.gameId card KingChooseHokmRequestCompleted Game.decoder
            )

        OnConnect ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Page Msg
view model =
    { title = "Find Game"
    , content =
        case model.session of
            Guest _ ->
                text "Sign in"

            LoggedIn _ cred ->
                viewAuthenticated cred model
    }


viewAuthenticated : Auth.Credential -> Model -> Element Msg
viewAuthenticated cred model =
    case model.game of
        Nothing ->
            text "Loading..."

        Just game ->
            viewGame cred game


viewGame : Auth.Credential -> Game -> Element Msg
viewGame cred game =
    case game of
        Game.NotFull g ->
            viewGameNotFull cred g

        Game.ChooseHokm g ->
            viewChooseHokm cred g

        Game.Started g ->
            viewGameStarted cred g


viewGameNotFull : Auth.Credential -> Game.NotFullInternal -> Element Msg
viewGameNotFull cred game =
    Element.column [] <| List.map text game.joinedPlayers


viewChooseHokm : Auth.Credential -> Game.ChooseHokmInternal -> Element Msg
viewChooseHokm cred game =
    Element.column []
        [ el [] <| text "Players"
        , Element.row [] <| List.map (\p -> text p.username) game.players
        , el [] <| text "Hakem"
        , text <| String.fromInt game.king
        , case Game.king game of
            Just kingUsername ->
                if Auth.username cred == kingUsername then
                    Element.row [] <| List.map viewSuit Card.suits

                else
                    text "you are not hakem"

            Nothing ->
                text "wtf2"
        ]


viewGameStarted : Auth.Credential -> Game.StartedInternal -> Element Msg
viewGameStarted cred game =
    Element.column [spacing 30]
        [ text "Player cards :"
        , viewUserCards (Auth.username cred) game.hands
        , text "Middle Cards:"
        , el [] <| text "Players"
        , Element.column [ spacing 20 ] <| List.map (viewPlayer game) game.players
        ]


viewUserCards : String -> Game.Hands -> Element Msg
viewUserCards username hands =
    case Dict.get username hands of
        Nothing ->
            Element.none

        Just cards ->
            View.Card.list (Just PlayCard) cards


viewPlayer : Game.StartedInternal -> Game.Player -> Element Msg
viewPlayer game player =
    case Dict.get player.username game.hands of
        Nothing ->
            text "wtf3"

        Just cards ->
            Element.column [ spacing 5 ]
                [ text player.username
                , View.Card.list Nothing cards
                ]


viewSuit : Card.Suit -> Element Msg
viewSuit suit =
    View.Card.item (Just (\c -> KingChooseHokm c.suit)) { suit = suit, value = Card.Ace }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ StoreCredential.changes GotSession (Session.navKey model.session)
        , Sub.map maybeMsg <| Socket.onConnectFrom (socketUrl model.gameId) OnConnect
        , Sub.map maybeMsg <| Socket.onMessagesFrom (socketUrl model.gameId) Game.decoder GotGame
        ]


toSession : Model -> Session
toSession model =
    model.session
