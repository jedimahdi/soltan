module Page.Home exposing (..)

import Data.Authentication exposing (Credential)
import Data.Card as Card
import Data.Session as Session exposing (Session)
import Effect.Api as Api
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Page exposing (Page)
import View.Card


type alias Model =
    { session : Session
    }


type Msg
    = GotSession Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )


view : Model -> Page Msg
view model =
    { title = "Home"
    , content =
        Element.column []
            [ el [] <| text "Home page!"
            , View.Card.item Nothing { suit = Card.Diamond, value = Card.Ace }
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    StoreCredential.changes GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
