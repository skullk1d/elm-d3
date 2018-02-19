module App.Page.About exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Font as Font


-- MESSAGES


type Msg
    = NoOp



-- STYLE


type Styles
    = None
    | Link
    | Description


styles : List (Style Styles variation)
styles =
    [ style None []
    , style Description
        [ Font.light
        , Font.size 32
        , Font.center 
        ]
    ]



-- VIEW


view : Element Styles variation Msg
view =
    column None
        []
        [ paragraph Description
            [ width fill ]
            [ el None [] (text "This is a demo showing data visualization with D3 v4 via Elm & TypeScript interop")
            ]
        ]



-- UPDATE


update : Msg -> Cmd Msg
update msg =
    case msg of
        NoOp ->
            Cmd.none
