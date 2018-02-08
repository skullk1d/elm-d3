module App.Page.NotFound exposing (..)

import Element exposing (..)
import Style exposing (..)


-- STYLE


type Styles
    = None


styles : List (Style Styles variation)
styles =
    [ Style.style None [] ]



-- VIEW


view : Element Styles variation msg
view =
    column None
        []
        [ el None [] (text "Not Found") ]
