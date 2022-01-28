module Main exposing (..)

import App
import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Authentication exposing (Credential)
import Data.Session as Session exposing (Session)
import Effect.Store.Credential as StoreCredential
import Html exposing (..)
import Json.Decode exposing (Value)
import Page
import Page.Blank as Blank
import Page.FindGame as FindGame
import Page.Game as Game
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Route exposing (Route)
import Url exposing (Url)


main : Program Value Model Msg
main =
    App.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotFindGameMsg FindGame.Msg
    | GotGameMsg Game.Msg
    | GotSession Session


type Model
    = NotFound Session
    | Redirect Session
    | Home Home.Model
    | Login Login.Model
    | FindGame FindGame.Model
    | Game Game.Model


init : Maybe Credential -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeCred url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.mk navKey maybeCred))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotFindGameMsg subMsg, FindGame findGame ) ->
            FindGame.update subMsg findGame
                |> updateWith FindGame GotFindGameMsg model

        ( GotGameMsg subMsg, Game game ) ->
            Game.update subMsg game
                |> updateWith Game GotGameMsg model

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.cred (toSession model)

        viewPage toMsg config =
            let
                { title, body } =
                    Page.view viewer config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view viewer Blank.view

        NotFound _ ->
            Page.view viewer NotFound.view

        Home home ->
            viewPage GotHomeMsg (Home.view home)

        Login login ->
            viewPage GotLoginMsg (Login.view login)

        FindGame findGame ->
            viewPage GotFindGameMsg (FindGame.view findGame)

        Game game ->
            viewPage GotGameMsg (Game.view game)


toSession : Model -> Session
toSession page =
    case page of
        NotFound session ->
            session

        Redirect session ->
            session

        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        FindGame findGame ->
            FindGame.toSession findGame

        Game game ->
            Game.toSession game


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Logout ->
            ( model, StoreCredential.logout )

        Just Route.FindGame ->
            FindGame.init session
                |> updateWith FindGame GotFindGameMsg model

        Just (Route.Game gameId) ->
            Game.init session gameId
                |> updateWith Game GotGameMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            StoreCredential.changes GotSession (Session.navKey (toSession model))

        NotFound _ ->
            Sub.none

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        FindGame findGame ->
            Sub.map GotFindGameMsg (FindGame.subscriptions findGame)

        Game game ->
            Sub.map GotGameMsg (Game.subscriptions game)
