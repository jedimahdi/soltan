module Page.NotFound exposing (view)

import Element exposing (el, text)
import Page exposing (Page)


view : Page msg
view =
    { title = "Page Not Found"
    , content =
        el [] <| text "Not Found"
    }
