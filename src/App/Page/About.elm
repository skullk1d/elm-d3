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
        [ Font.light, Font.size 32, Font.center ]
    ]



-- VIEW


view : Element Styles variation Msg
view =
    column None
        []
        [ el None [] (text "About")
        , paragraph Description
            [ width fill ]
            [ el None [] (text "This is a demo showing data visualization with D3 l via Elm interoperability with TypeScript")
            ]
        ]



-- UPDATE


update : Msg -> Cmd Msg
update msg =
    case msg of
        NoOp ->
            Cmd.none
