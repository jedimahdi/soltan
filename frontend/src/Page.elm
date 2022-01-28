module Page exposing (..)

import Browser exposing (Document)
import Data.Authentication as Auth exposing (Credential)
import Element exposing (Element, alignLeft, alignRight, centerX, el, fill, height, maximum, padding, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Route exposing (Route)


type alias Page msg =
    { title : String, content : Element msg, bg : Element.Color }


view : Maybe Credential -> Page msg -> Document msg
view maybeCred { title, content, bg } =
    { title = title ++ " - Soltan"
    , body =
        [ Element.layout [ Background.color bg ] <|
            Element.column [ width fill ] [ viewHeader maybeCred, el [ width (fill |> maximum 1200), centerX ] content ]
        ]
    }


viewHeader : Maybe Credential -> Element msg
viewHeader maybeCred =
    el [ Background.color (Element.rgb255 51 56 59), Font.color (Element.rgb255 131 145 151), width fill, height (px 50), Border.shadow { offset = ( 0, 0 ), size = 1, blur = 5, color = Element.rgba255 0 0 0 0.8 } ] <|
        Element.row [ spacing 10, Element.paddingXY 10 0, width (fill |> maximum 1200), centerX ] <|
            [ el [ Font.size 28 ] <| text "Soltan"
            , Element.row [ width fill ] <|
                case maybeCred of
                    Just cred ->
                        [ navLink Route.Home "Home"
                        , navLink Route.FindGame "Find game"
                        , el [ alignRight ] <| text <| Auth.username cred
                        , navLink Route.Logout "Logout"
                        ]

                    Nothing ->
                        [ navLink Route.Home "Home"
                        , navLink Route.Login "Sign in"
                        ]
            ]


navLink : Route -> String -> Element msg
navLink route label =
    Route.link [ padding 15, Element.mouseOver [ Font.color (Element.rgb255 161 175 181) ] ] { route = route, label = text label }
