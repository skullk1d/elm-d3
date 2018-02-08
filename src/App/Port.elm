port module App.Port exposing (..)

import Json.Decode as Decode exposing (list)
import Json.Encode as Encode exposing (list)
import Result exposing (Result(Ok, Err))
import Dict exposing (Dict)
import Array exposing (Array)
import App.Data exposing (..)
import App.Request exposing (getDataPointAnalyses, filterAnalysesByTime)
import RemoteData exposing (..)


type alias Model =
    { api : String
    , partitions : List Partition
    , partitionParams : PartitionParams
    , dataPointAnalysesResponse : WebData PartitionDatum
    , dataPointAnalyses : Encode.Value
    }


type Msg
    = NoOp
    | GetDataPointAnalysesResponse (WebData PartitionDatum)
    | Outside IncomingMsg
    | LogError String


init : String -> ( Model, Cmd Msg )
init api =
    let
        initPartitionParams =
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

        initModel =
            { api = api
            , partitions = []
            , dataPointAnalyses = Encode.array Array.empty
            , dataPointAnalysesResponse = NotAsked
            , partitionParams = initPartitionParams
            }
    in
        initModel
            ! [ getDataPointAnalyses GetDataPointAnalysesResponse initModel.api
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetDataPointAnalysesResponse res ->
            let
                dataPointAnalyses =
                    case res of
                        NotAsked ->
                            model.dataPointAnalyses

                        Loading ->
                            model.dataPointAnalyses

                        Success (Datum analyses) ->
                            case analyses.children of
                                Just children ->
                                    filterAnalysesByTime children
                                        { start = 1508684400000
                                        , end = 1509116400000
                                        , unit = 3600000
                                        , timeFrame = Day
                                        }
                                        |> encodePartitionData

                                Nothing ->
                                    Encode.array Array.empty

                        Failure _ ->
                            Encode.array Array.empty

                partitionParams =
                    model.partitionParams

                newParams =
                    { partitionParams | data = dataPointAnalyses }
            in
                ( { model | dataPointAnalyses = dataPointAnalyses, partitionParams = newParams }
                , dispatchOutgoingMsg (GeneratePartitionMap newParams)
                )

        Outside incomingMsg ->
            case incomingMsg of
                GeneratedPartitionMap partitions ->
                    ( { model | partitions = partitions }
                    , Cmd.none
                    )

        LogError err ->
            model ! [ dispatchOutgoingMsg (LogErrorOutside (toString err)) ]



-- Actor model


type OutgoingMsg
    = GeneratePartitionMap PartitionParams
    | LogErrorOutside String


type IncomingMsg
    = GeneratedPartitionMap (List Partition)


port outgoingData : OutsideData -> Cmd msg


port incomingData : (OutsideData -> msg) -> Sub msg


dispatchOutgoingMsg : OutgoingMsg -> Cmd msg
dispatchOutgoingMsg outMsg =
    case outMsg of
        GeneratePartitionMap params ->
            outgoingData { tag = "GeneratePartitionMap", data = encodePartitionParams params }

        LogErrorOutside str ->
            outgoingData { tag = "LogErrorOutside", data = Encode.string ("Unexpected outgoing message: " ++ str) }


acceptIncomingMsg : (IncomingMsg -> msg) -> (String -> msg) -> Sub msg
acceptIncomingMsg tagger onError =
    incomingData
        (\outsideData ->
            case outsideData.tag of
                "GeneratedPartitionMap" ->
                    case Decode.decodeValue (Decode.list partitionDecoder) outsideData.data of
                        Ok partitions ->
                            let
                                _ =
                                    Debug.log "incomingData GeneratedPartitionMap" partitions
                            in
                                tagger <| GeneratedPartitionMap partitions

                        Err err ->
                            onError err

                _ ->
                    onError <| "Unexpected tag from outside: " ++ outsideData.tag
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ acceptIncomingMsg Outside LogError
        ]
