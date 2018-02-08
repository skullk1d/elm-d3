module App.View exposing (..)

import Html exposing (..)
import Element exposing (..)
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
    | TopStyles Top.Styles
    | AboutStyles About.Styles
    | NotFoundStyles NotFound.Styles
    | MenuStyle Menu.Styles


type Variations
    = MenuVariation Menu.Variations


styles : List (Style Styles Variations)
styles =
    let
        map toStyle toVariation =
            Sheet.map toStyle toVariation >> Sheet.merge
    in
        [ Style.style None []
        , map TopStyles identity Top.styles
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
        column None
            []
            [ Menu.view { toMsg = MenuMsg } model.menu
                |> mapAll identity MenuStyle MenuVariation
            , (routeOutlet model)
            ]


routeOutlet : Model -> Element Styles variation Msg
routeOutlet model =
    case model.currentRoute of
        Routing.TopRoute ->
            Top.view { newRoute = NewRoute, toMsg = TopMsg } model.top
                |> mapAll identity TopStyles identity

        Routing.AboutRoute ->
            About.view { newRoute = NewRoute, toMsg = AboutMsg }
                |> mapAll identity AboutStyles identity

        Routing.NotFoundRoute ->
            NotFound.view
                |> mapAll identity NotFoundStyles identity
