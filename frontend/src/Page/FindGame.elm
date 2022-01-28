module Page.FindGame exposing (..)

import Data.Authentication as Auth
import Data.Game as Game exposing (Game)
import Data.Session exposing (..)
import Effect.Api as Api
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, px, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Page exposing (Page)
import Route exposing (Route)


type alias Model =
    { session : Session
    }


type Msg
    = GotSession Session
    | FindGameClicked
    | CompletedFindingGame (Result Http.Error Game)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


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
        FindGameClicked ->
            ( model
            , Api.findGame cred CompletedFindingGame Game.decoder
            )

        CompletedFindingGame (Ok game) ->
            ( model
            , Route.replaceUrl (navKey model.session) (Route.Game (Game.id game))
            )

        CompletedFindingGame (Err _) ->
            ( model
            , Cmd.none
            )

        GotSession session ->
            ( { model | session = session }, Cmd.none )


view : Model -> Page Msg
view model =
    { title = "Find Game"
    , bg = Element.rgb255 87 113 84
    , content =
        el [ Element.paddingEach { top = 30, right = 0, left = 0, bottom = 0 }, Font.color (Element.rgb255 131 145 151) ] <|
            case model.session of
                Guest _ ->
                    text "Sign in"

                LoggedIn _ cred ->
                    viewAuthenticated cred model
    }


viewAuthenticated : Auth.Credential -> Model -> Element Msg
viewAuthenticated cred model =
    Input.button
        [ Background.color <| Element.rgb255 70 90 78
        , Font.color <| Element.rgb255 255 255 255
        , Element.width (px 300)
        , Element.height (px 50)
        , Border.rounded 5
        ]
        { label = el [ Element.centerX ] <| text "2v2", onPress = Just FindGameClicked }


subscriptions : Model -> Sub Msg
subscriptions model =
    StoreCredential.changes GotSession (navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
