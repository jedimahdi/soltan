module Data.Session exposing (..)

import Browser.Navigation as Nav
import Data.Authentication as Auth exposing (Credential)


type Session
    = LoggedIn Nav.Key Credential
    | Guest Nav.Key


cred : Session -> Maybe Credential
cred session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


username : Session -> Maybe String
username session =
    case session of
        LoggedIn _ val ->
            Just (Auth.username val)

        Guest _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


mk : Nav.Key -> Maybe Credential -> Session
mk key maybeViewer =
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
