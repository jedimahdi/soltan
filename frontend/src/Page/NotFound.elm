module Page.NotFound exposing (view)

import Element exposing (el, text)
import Page exposing (Page)


view : Page msg
view =
    { title = "Page Not Found"
    , bg = Element.rgb255 57 62 66
    , content =
        el [] <| text "Not Found"
    }
