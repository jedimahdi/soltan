module Page.FindGame exposing (..)

import Data.Authentication as Auth
import Data.Game as Game exposing (Game)
import Data.Session exposing (..)
import Effect.Api as Api
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, text)
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
    , content =
        case model.session of
            Guest _ ->
                text "Sign in"

            LoggedIn _ cred ->
                viewAuthenticated cred model
    }


viewAuthenticated : Auth.Credential -> Model -> Element Msg
viewAuthenticated cred model =
    Input.button [] { label = text "2v2", onPress = Just FindGameClicked }


subscriptions : Model -> Sub Msg
subscriptions model =
    StoreCredential.changes GotSession (navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
