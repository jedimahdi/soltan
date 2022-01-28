module Page.Blank exposing (view)

import Element
import Page exposing (Page)


view : Page msg
view =
    { title = ""
    , content = Element.text ""
    , bg = Element.rgb255 57 62 66
    }
