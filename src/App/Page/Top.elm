module App.Page.Top exposing (..)

import Navigation exposing (Location)
import Color exposing (..)
import Element exposing (..)
import Element.Input exposing (..)
import Element.Attributes exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Sheet as Sheet
import App.Util exposing (onPreventDefaultClick)
import Task
import RemoteData exposing (..)
import App.Request exposing (getDataPoints)
import App.Routing as Routing exposing (Route(..))
import App.Data exposing (DataPoint, Partition, PartitionNode, PartitionParams, PartitionShape(..), PartitionForm)
import App.View.DataTable as DataTable
import App.View.PartitionMap as PartitionMap


-- MODEL


type alias Model =
    { dataPointsResponse : WebData (List DataPoint)
    , partitions : List Partition
    , partitionWidth : Int
    , partitionHeight : Int
    , selectMenu : SelectWith PartitionShape Msg
    }



-- MESSAGES


type Msg
    = NoOp
    | NewRoute Route
    | GetDataPointsResponse (WebData (List DataPoint))
    | SelectOne (SelectMsg PartitionShape)
    | SubmitForm PartitionForm



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


view : Model -> Element Styles variation Msg
view model =
    let
        text =
            Element.text
    in
        column None
            []
            [ el None [] (text "Data View")
            , paragraph None
                []
                [ link (Routing.routeToUrl AboutRoute) <|
                    el Link [ onPreventDefaultClick <| NewRoute AboutRoute ] (text "About")
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
            , select None
                [ padding 10
                , spacing 20
                ]
                { label = labelAbove <| text "Shape:"
                , with = model.selectMenu
                , max = 5
                , options = []
                , menu =
                    menuAbove None
                        []
                        [ choice Arc (text "Arc")
                        , choice Rectangle (text "Rectangle")
                        ]
                }
            , row None
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
    , selectMenu = dropMenu (Just Arc) SelectOne
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewRoute route ->
            ( model, Navigation.newUrl (Routing.routeToUrl route) )

        GetDataPointsResponse response ->
            ( { model | dataPointsResponse = response }
            , Cmd.none
            )

        SelectOne selectMsg ->
            let
                selectMenu =
                    updateSelection selectMsg model.selectMenu

                shape =
                    selected selectMenu
                        |> Maybe.withDefault Arc
            in
                -- no submit button so we submit as soon as select form changes
                ( { model | selectMenu = selectMenu }
                , Task.succeed (SubmitForm { shape = shape }) |> Task.perform identity
                )

        SubmitForm _ ->
            ( model, Cmd.none )


onNavigate : Model -> String -> ( Model, Cmd Msg )
onNavigate model api =
    { model | dataPointsResponse = Loading }
        ! [ getDataPoints GetDataPointsResponse api ]
