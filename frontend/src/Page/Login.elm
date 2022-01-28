module Page.Login exposing (..)

import Browser.Navigation as Nav
import Data.Authentication as Auth exposing (Credential)
import Data.Session as Session exposing (Session)
import Effect.Api as Api
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, px, rgb255, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Page exposing (Page)
import Route exposing (Route)


type alias Model =
    { session : Session
    , problems : List Problem
    , form : Form
    }


type Msg
    = SubmittedForm
    | EnteredUsername String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error Credential)
    | GotSession Session


type alias Form =
    { username : String
    , password : String
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type ValidatedField
    = Username
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Password
    ]


type TrimmedForm
    = Trimmed Form


trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { username = String.trim form.username
        , password = String.trim form.password
        }


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.username then
                    [ "username can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , form = { username = "", password = "" }
      , problems = []
      }
    , Cmd.none
    )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredUsername username ->
            updateForm (\form -> { form | username = username }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        SubmittedForm ->
            case validate model.form of
                Ok _ ->
                    ( { model | problems = [] }
                    , Api.login model.form.username model.form.password CompletedLogin Auth.decoder
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        CompletedLogin (Err error) ->
            let
                serverErrors =
                    Api.decodeErrors error
                        |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedLogin (Ok viewer) ->
            ( model
            , StoreCredential.storeCredWith viewer
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )


view : Model -> Page Msg
view model =
    { title = "Login"
    , bg = Element.rgb255 57 62 66
    , content =
        Element.column [ Element.paddingEach { top = 30, right = 0, left = 0, bottom = 0 }, Font.color (Element.rgb255 131 145 151) ]
            [ Element.column [] <| List.map viewProblem model.problems
            , viewForm model.form
            ]
    }


viewProblem : Problem -> Element msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    text errorMessage


viewForm : Form -> Element Msg
viewForm form =
    Element.column [ Element.spacing 20 ]
        [ el [] <| text "Fill your information"
        , Input.username [ Background.color (rgb255 51 56 59), Border.color (rgb255 41 46 49), Border.rounded 5 ]
            { onChange = EnteredUsername
            , text = form.username
            , placeholder = Just <| Input.placeholder [] (text "Username")
            , label = Input.labelHidden "Username"
            }
        , Input.newPassword [ Background.color (rgb255 51 56 59), Border.color (rgb255 41 46 49), Border.rounded 5 ]
            { onChange = EnteredPassword
            , text = form.password
            , placeholder = Just <| Input.placeholder [] (text "Password")
            , label = Input.labelHidden "Password"
            , show = False
            }
        , Input.button [ Background.color (rgb255 153 47 83), Font.color (rgb255 220 220 220), Element.width (px 300), Element.height (px 50), Border.rounded 5, Element.mouseOver [ Background.color (rgb255 158 52 88) ] ]
            { onPress = Just SubmittedForm
            , label = el [ Element.centerX ] <| text "Enter"
            }
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    StoreCredential.changes GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
