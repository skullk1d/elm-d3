module App.View exposing (..)

import Html exposing (..)
import Element exposing (..)
import Style.Font as Font
import Style exposing (..)
import Style.Sheet as Sheet
import App.State exposing (..)
import App.Page.Top as Top
import App.Page.About as About
import App.Page.NotFound as NotFound
import App.View.Menu as Menu
import App.Routing as Routing


-- STYLE


type Styles
    = None
    | GlobalStyles
    | TopStyles Top.Styles
    | AboutStyles About.Styles
    | NotFoundStyles NotFound.Styles
    | MenuStyle Menu.Styles


type Variations
    = MenuVariation Menu.Variations
    | TopVariation Top.Variations


styles : List (Style Styles Variations)
styles =
    let
        map toStyle toVariation =
            Sheet.map toStyle toVariation >> Sheet.merge
    in
        [ Style.style None []
        , style GlobalStyles
            [ Font.typeface
                [ Font.font "helvetica"
                , Font.font "arial"
                , Font.font "sans-serif"
                ]
            ]
        , map TopStyles TopVariation Top.styles
        , map AboutStyles identity About.styles
        , map NotFoundStyles identity NotFound.styles
        , map MenuStyle MenuVariation Menu.styles
        ]


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet styles



-- VIEW


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        column GlobalStyles
            []
            [ Element.map MenuMsg (Menu.view model.currentRoute
                |> mapAll identity MenuStyle MenuVariation)
            , (routeOutlet model)
            ]


routeOutlet : Model -> Element Styles Variations Msg
routeOutlet model =
    case model.currentRoute of
        Routing.TopRoute ->
            Element.map TopMsg
                (Top.view model.top
                    |> mapAll identity TopStyles TopVariation
                )

        Routing.AboutRoute ->
            Element.map AboutMsg
                (About.view
                    |> mapAll identity AboutStyles identity
                )

        Routing.NotFoundRoute ->
            NotFound.view
                |> mapAll identity NotFoundStyles identity
                
