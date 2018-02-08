module App.View.DataTable exposing (..)

-- import Html exposing (..)
-- import Html.Attributes exposing (class)

import Element exposing (..)
import Style exposing (..)
import Style.Font as Font
import App.Data exposing (DataPoint)


-- STYLE


type Styles
    = None
    | Title


styles : List (Style Styles variation)
styles =
    [ Style.style None []
    , Style.style Title
        [ Font.size 32 ]
    ]



-- VIEW


dataPointTable : List DataPoint -> Element Styles variation msg
dataPointTable dataPoints =
    column None
        []
        [ h1 Title [] (text "Raw data")
        , table None
            []
            [ (List.concat [ [ text "Id" ], (List.map (\hr -> text (toString hr.id)) dataPoints) ])
            , (List.concat [ [ text "Time" ], (List.map (\hr -> text (toString hr.time)) dataPoints) ])
            , (List.concat [ [ text "Kind" ], (List.map (\hr -> text (toString hr.kind)) dataPoints) ])
            , (List.concat [ [ text "Value" ], (List.map (\hr -> text (toString hr.value)) dataPoints) ])
            ]
        ]
