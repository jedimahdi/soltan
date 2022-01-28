module Page.Home exposing (..)

import Data.Authentication exposing (Credential)
import Data.Card as Card
import Data.Session as Session exposing (Session)
import Effect.Api as Api
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, text)
import Element.Font as Font
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
    , bg = Element.rgb255 57 62 66
    , content =
        Element.column [ Element.paddingEach { top = 30, right = 0, left = 0, bottom = 0 }, Font.color (Element.rgb255 131 145 151) ]
            [ el [] <| text "Home page!"
            ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    StoreCredential.changes GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
