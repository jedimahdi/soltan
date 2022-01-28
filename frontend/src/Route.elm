module Route exposing (..)

import Browser.Navigation as Nav
import Element exposing (Element)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Login
    | Logout
    | FindGame
    | Game String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map FindGame (s "game" </> s "find")
        , Parser.map Game (s "game" </> string)
        ]


link :
    List (Element.Attribute msg)
    ->
        { route : Route
        , label : Element msg
        }
    -> Element msg
link attrs d =
    Element.link attrs { url = routeToString d.route, label = d.label }


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Login ->
            [ "login" ]

        Logout ->
            [ "logout" ]

        FindGame ->
            [ "game", "find" ]

        Game gameId ->
            [ "game", gameId ]
