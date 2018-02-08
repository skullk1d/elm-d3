module App.Page.About exposing (..)

import Color exposing (..)
import Element exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import App.Util exposing (onPreventDefaultClick)
import App.Routing as Routing exposing (Route(..))


-- MESSAGES


type Msg
    = NoOp



-- STYLE


type Styles
    = None
    | Link


styles : List (Style Styles variation)
styles =
    [ Style.style None []
    , Style.style Link
        [ Color.text green, Font.underline ]
    ]



-- VIEW


view : { a | newRoute : Route -> msg, toMsg : Msg -> msg } -> Element Styles variation msg
view { newRoute, toMsg } =
    column None
        []
        [ el None [] (text "About")
        , paragraph None
            []
            [ link (Routing.routeToUrl TopRoute) <|
                el Link [ onPreventDefaultClick <| newRoute TopRoute ] (text "top")
            ]
        , paragraph None
            []
            [ el None [] (text "This is a demo showing data visualization with D3 l via Elm interoperability with TypeScript")
            ]
        ]



-- UPDATE


update : Msg -> Cmd Msg
update msg =
    case msg of
        NoOp ->
            Cmd.none
