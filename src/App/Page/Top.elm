module App.Page.Top exposing (..)

import Color exposing (..)
import Element exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Sheet as Sheet
import App.Util exposing (onPreventDefaultClick)
import RemoteData exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)
import Json.Encode as Encode exposing (list)
import App.Request exposing (getDataPoints)
import App.Routing as Routing exposing (Route(..))
import App.Data exposing (DataPoint, Partition, PartitionNode, PartitionParams, PartitionShape(..))
import App.View.DataTable as DataTable
import App.View.PartitionMap as PartitionMap


-- MODEL


type alias Model =
    { dataPointsResponse : WebData (List DataPoint)
    , partitions : List Partition
    , partitionWidth : Int
    , partitionHeight : Int
    , partitionParams : PartitionParams
    }



-- MESSAGES


type Msg
    = NoOp
    | GetDataPointsResponse (WebData (List DataPoint))



-- STYLE


type Styles
    = None
    | Link
    | DataPointTableStyles DataTable.Styles
    | PartitionMapStyles PartitionMap.Styles


styles : List (Style Styles variation)
styles =
    let
        map toStyle =
            Sheet.map toStyle identity >> Sheet.merge
    in
        [ Style.style None []
        , Style.style Link
            [ Color.text red, Font.underline ]
        , map DataPointTableStyles DataTable.styles
        ]



-- VIEW


view : { a | newRoute : Route -> msg, toMsg : Msg -> msg } -> Model -> Element Styles variation msg
view { newRoute, toMsg } model =
    column None
        []
        [ el None [] (text "Data View")
        , paragraph None
            []
            [ link (Routing.routeToUrl AboutRoute) <|
                el Link [ onPreventDefaultClick <| newRoute AboutRoute ] (text "About")
            ]
        , el None
            []
            (case model.dataPointsResponse of
                NotAsked ->
                    text ""

                Loading ->
                    text "Getting data..."

                Failure err ->
                    text ("Error: " ++ toString err)

                Success dataPoints ->
                    text "Got data"
            )
        , model.dataPointsResponse
            |> RemoteData.map
                (\dataPoints ->
                    DataTable.dataPointTable dataPoints
                        |> mapAll identity DataPointTableStyles identity
                )
            |> RemoteData.withDefault
                (el None [] (empty))
        , h3 None [] (text "Arcs")
        , row
            None
            []
            (List.map
                (\partition ->
                    (PartitionMap.view partition model.partitionWidth model.partitionHeight
                        |> mapAll identity PartitionMapStyles identity
                    )
                )
                model.partitions
            )
        ]



-- INIT


init : Model
init =
    { dataPointsResponse = NotAsked
    , partitions = []
    , partitionWidth = 0
    , partitionHeight = 0
    , partitionParams =
        { colorMap =
            Dict.fromList
                [ ( "blue", "#3c7df3" )
                , ( "red", "#f06292" )
                , ( "green", "#90eb9d" )
                ]
        , data = Encode.array Array.empty
        , shape = Arc
        , width = 300
        , height = 300
        }
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetDataPointsResponse response ->
            ( { model | dataPointsResponse = response }
            , Cmd.none
            )


onNavigate : Model -> String -> ( Model, Cmd Msg )
onNavigate model api =
    { model | dataPointsResponse = Loading }
        ! [ getDataPoints GetDataPointsResponse api ]
