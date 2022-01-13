module Page.Login exposing (..)

import Browser.Navigation as Nav
import Data.Authentication as Auth exposing (Credential)
import Data.Session as Session exposing (Session)
import Effect.Api as Api
import Effect.Store.Credential as StoreCredential
import Element exposing (Element, el, text)
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
    , content =
        Element.column []
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
    Element.column []
        [ Input.username []
            { onChange = EnteredUsername
            , text = form.username
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Username"
            }
        , Input.newPassword []
            { onChange = EnteredPassword
            , text = form.password
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Password"
            , show = False
            }
        , Input.button []
            { onPress = Just SubmittedForm
            , label = text "Sign in"
            }
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    StoreCredential.changes GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
