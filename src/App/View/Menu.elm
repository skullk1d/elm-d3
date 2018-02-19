module App.View.Menu exposing (..)

-- import Html exposing (..)
-- import Html.Attributes exposing (class)

import Navigation exposing (Location)
import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Border as Border
import App.Util exposing (onPreventDefaultClick)
import App.Routing as Routing exposing (Route(..))


type Msg
    = NoOp
    | NewRoute Route



-- STYLE


type Styles
    = None
    | Link


type Variations
    = Selected


styles : List (Style Styles Variations)
styles =
    [ style None []
    , style Link
        [ Color.text lightCharcoal
        , variation Selected 
            [ Color.text darkRed
            , Border.solid
            , Border.bottom 2
            ]
        , hover
            [ Color.text lightRed
            ]
        ]
    ]


-- VIEW


view : Route -> Element Styles Variations Msg
view currentRoute =
    let
        buttonAttr = 
            [ padding 10
            , spacing 10
            ]
    in
    
        column None []
            [ row None []
                [ link (Routing.routeToUrl AboutRoute) <|
                    button Link
                        (List.append 
                            [ onPreventDefaultClick <| NewRoute AboutRoute
                            , vary Selected (currentRoute == AboutRoute)
                            ] buttonAttr
                        )
                        (text "About")
                , link (Routing.routeToUrl TopRoute) <|
                    button Link
                        (List.append
                            [ onPreventDefaultClick <| NewRoute TopRoute
                            , vary Selected (currentRoute == TopRoute)
                            ] buttonAttr
                        )
                        (text "Data")
                ]
            ]


-- UPDATE


update : Msg -> Cmd Msg
update msg =
    case msg of
        NoOp ->
            Cmd.none

        NewRoute route ->
            Navigation.newUrl (Routing.routeToUrl route)
