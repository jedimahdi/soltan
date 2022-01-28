module Page.Game exposing (..)

import Data.Authentication as Auth exposing (Credential)
import Data.Card as Card exposing (Card)
import Data.Game as Game exposing (Game)
import Data.Session as Session exposing (..)
import Dict
import Effect.Api as Api
import Effect.Socket as Socket
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, px, spacing, text)
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
            , Api.playCard cred model.gameId card PlayCardRequestCompleted Game.decoder
            )

        OnConnect ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Page Msg
view model =
    { title = "Find Game"
    , bg = Element.rgb255 87 113 84
    , content =
        el [ Element.paddingEach { top = 30, right = 0, left = 0, bottom = 0 }, Font.color (Element.rgb255 0 0 0), Element.width Element.fill ] <|
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
            viewGameNotFull g

        Game.ChooseHokm g ->
            viewChooseHokm cred g

        Game.Started g ->
            viewGameStarted cred g


viewGameNotFull : Game.NotFullInternal -> Element Msg
viewGameNotFull game =
    Element.column [ spacing 15, Element.centerX, Font.size 24 ]
        [ el [ Element.centerX ] <| text "Finding Opponents..."
        , el [ Element.centerX ] <| text <| String.fromInt (List.length game.joinedPlayers) ++ "  of  4"
        ]


viewChooseHokm : Auth.Credential -> Game.ChooseHokmInternal -> Element Msg
viewChooseHokm cred game =
    Element.column [ spacing 50, Element.width Element.fill ]
        [ if Auth.username cred == game.king then
            Element.column [ spacing 30, Element.centerX ]
                [ text "You're the Hakem, choose a hokm:"
                , Element.row
                    [ spacing 10 ]
                  <|
                    List.map viewSuit Card.suits
                ]

          else
            Element.column [ Element.centerX ]
                [ text "Hakem is choosing hokm..."
                ]
        , el
            [ Background.color <| Element.rgb255 70 90 78
            , Element.width Element.fill
            , Element.padding 40
            , Border.rounded 5
            ]
          <|
            View.Card.list [ spacing 10, Element.centerX ] { cards = game.cards, onClick = Nothing }
        ]


viewGameStarted : Auth.Credential -> Game.StartedInternal -> Element Msg
viewGameStarted cred game =
    Element.column [ spacing 30 ]
        [ viewMiddleCards cred game.players
        , el
            [ Background.color <| Element.rgb255 70 90 78
            , Element.width Element.fill
            , Element.padding 40
            , Border.rounded 5
            ]
          <|
            View.Card.list [ spacing 10, Element.centerX ] { cards = game.cards, onClick = Just PlayCard }
        ]


viewMiddleCards : Auth.Credential -> List Game.Player -> Element Msg
viewMiddleCards cred players =
    Element.column [ Element.width Element.fill ] <|
        viewPlayers players


viewPlayers : List Game.Player -> List (Element Msg)
viewPlayers players =
    case players of
        [ p1, p2, p3, p4 ] ->
            [ el [ Element.centerX ] <| viewPlayer p3
            , Element.row [ spacing 180, Element.centerX ] (List.map viewPlayer [ p4, p2 ])
            , el [ Element.centerX ] <| viewPlayer p1
            ]

        _ ->
            []


viewPlayer : Game.Player -> Element Msg
viewPlayer player =
    Element.column [ spacing 10 ]
        [ text player.username
        , case player.playedCard of
            Nothing ->
                el
                    [ Element.width (px 100)
                    , Element.height (px 144)
                    , Border.rounded 5
                    , Background.color <| Element.rgb255 70 90 78
                    ]
                    Element.none

            Just card ->
                View.Card.item Nothing card
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
