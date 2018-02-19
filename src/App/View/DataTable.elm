module App.View.DataTable exposing (..)

-- import Html exposing (..)
-- import Html.Attributes exposing (class)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Font as Font
import Style.Color as Color
import App.Data exposing (DataPoint)


-- STYLE


type Styles
    = None
    | Row
    | Cell


type Variations
    = CellHeader


styles : List (Style Styles Variations)
styles =
    [ style None []
    , style Row
        [ pseudo "nth-child(even)"
            [ Color.background Color.lightGray ]
        ]
    , style Cell
        [ Font.size 16
        , variation CellHeader
            [ Font.bold
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            ]
        ]
    ]



-- VIEW


dataPointTable : List DataPoint -> Element Styles Variations msg
dataPointTable dataPoints =
    column None
        []
        [ table None
            []
            [ [ el Cell [ vary CellHeader True ] (text "Id") ]
            , [ el Cell [ vary CellHeader True ] (text "Time") ]
            , [ el Cell [ vary CellHeader True ] (text "Kind") ]
            , [ el Cell [ vary CellHeader True ] (text "Value") ]
            ]
        , column None
            []
            (List.map
                (\d ->
                    row Row
                        []
                        [ el Cell [ width fill, alignLeft ] (text (toString d.id))
                        , el Cell [ width fill, alignLeft ] (text (toString d.time))
                        , el Cell [ width fill, alignLeft ] (text (toString d.kind))
                        , el Cell [ width fill, alignLeft ] (text (toString d.value))
                        ]
                )
                dataPoints
            )
        ]
