module Page exposing (..)

import Browser exposing (Document)
import Data.Authentication as Auth exposing (Credential)
import Element exposing (Element, alignLeft, alignRight, centerX, el, fill, padding, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Route exposing (Route)


type alias Page msg =
    { title : String, content : Element msg }


view : Maybe Credential -> { title : String, content : Element msg } -> Document msg
view maybeCred { title, content } =
    { title = title ++ " - Hokm"
    , body =
        [ Element.layout [] <|
            Element.column [ spacing 10, width (px 700), centerX ] [ viewHeader maybeCred, content ]
        ]
    }


viewHeader : Maybe Credential -> Element msg
viewHeader maybeCred =
    Element.row [ spacing 5, width fill ] <|
        case maybeCred of
            Just cred ->
                [ navLink Route.Home "Home"
                , navLink Route.FindGame "Find game"
                , el [ alignRight ] <| text <| Auth.username cred
                ]

            Nothing ->
                [ navLink Route.Home "Home"
                , navLink Route.Login "Sign in"
                ]


navLink : Route -> String -> Element msg
navLink route label =
    Route.link [ padding 5 ] { route = route, label = text label }
