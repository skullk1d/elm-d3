module App.Data exposing (..)

import Time exposing (Time)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Array exposing (Array)


{- raw data -}


type DataPointType
    = Blue
    | Green
    | Red


type alias DataPoint =
    { id : Int
    , kind : DataPointType
    , time : Time
    , value : Int
    }


dataPointDecoder : Decoder DataPoint
dataPointDecoder =
    Decode.map4 DataPoint
        (Decode.field "id" Decode.int)
        (Decode.field "type" dataPointTypeDecoder)
        (Decode.field "time" Decode.float)
        (Decode.field "value" Decode.int)


dataPointTypeDecoder : Decoder DataPointType
dataPointTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "blue" ->
                        Decode.succeed Blue

                    "green" ->
                        Decode.succeed Green

                    "red" ->
                        Decode.succeed Red

                    _ ->
                        Decode.fail <| "Unknown data point type: " ++ str
            )


encodeDataPoint : DataPoint -> Value
encodeDataPoint dataPoint =
    Encode.object
        [ ( "id", Encode.int dataPoint.id )
        , ( "type", encodeDataPointType dataPoint.kind )
        , ( "time", encodeDataPointTime dataPoint.time )
        , ( "value", Encode.int dataPoint.value )
        ]


encodeDataPointType : DataPointType -> Value
encodeDataPointType hrt =
    Encode.string
        (case hrt of
            Blue ->
                "blue"

            Green ->
                "green"

            Red ->
                "red"
        )


encodeDataPointTime : Time -> Value
encodeDataPointTime time =
    Encode.float time



-- SVG data visualization


type alias Partition =
    { nodes : List PartitionNode
    , viewBox : String
    }


type PartitionNode
    = Path PartitionPath
    | Rect PartitionRect


type alias PartitionPath =
    { d : String
    , style : String
    , transform : String
    }


type alias PartitionRect =
    { x : String
    , y : String
    , width : String
    , height : String
    , style : String
    }


partitionPathDecoder : Decoder PartitionPath
partitionPathDecoder =
    Decode.map3 PartitionPath
        (Decode.field "d" Decode.string)
        (Decode.field "style" Decode.string)
        (Decode.field "transform" Decode.string)


partitionRectDecoder : Decoder PartitionRect
partitionRectDecoder =
    Decode.map5 PartitionRect
        (Decode.field "x" Decode.string)
        (Decode.field "y" Decode.string)
        (Decode.field "width" Decode.string)
        (Decode.field "height" Decode.string)
        (Decode.field "style" Decode.string)


partitionDecoder : Decoder Partition
partitionDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                let
                    partitionDecoder =
                        case tag of
                            "path" ->
                                Decode.at [ "nodes" ] (Decode.list partitionPathDecoder)
                                    |> Decode.map (List.map Path)

                            "rect" ->
                                Decode.at [ "nodes" ] (Decode.list partitionRectDecoder)
                                    |> Decode.map (List.map Rect)

                            _ ->
                                Decode.fail ("Unknown tag: " ++ tag)

                    viewBoxDecoder =
                        Decode.field "viewBox" Decode.string
                in
                    Decode.map2
                        (\nodes viewBox ->
                            { nodes = nodes
                            , viewBox = viewBox
                            }
                        )
                        partitionDecoder
                        viewBoxDecoder
            )



{- D3 data -}


type alias DatumAlias =
    { name : String
    , size : Maybe Int
    , others : Maybe (Dict String String)
    , children : Maybe (List PartitionDatum)
    }


type PartitionDatum
    = Datum DatumAlias


type PartitionShape
    = Rectangle
    | Arc


type alias PartitionParams =
    { colorMap : Dict String String
    , data : Decode.Value
    , shape : PartitionShape
    , width : Int
    , height : Int
    }


partitionDatumDecoder : Decoder PartitionDatum
partitionDatumDecoder =
    Decode.map Datum <|
        Decode.map4 DatumAlias
            (Decode.field "name" Decode.string)
            (Decode.maybe <| Decode.field "size" Decode.int)
            (Decode.maybe <| Decode.field "others" <| Decode.dict Decode.string)
            (Decode.maybe <| Decode.field "children" <| Decode.lazy (\_ -> Decode.list partitionDatumDecoder))


timeFrameDecoder : Decoder TimeFrame
timeFrameDecoder =
    Decode.string
        |> Decode.andThen
            (\tf ->
                case tf of
                    "raw" ->
                        Decode.succeed Raw

                    "day" ->
                        Decode.succeed Day

                    "week" ->
                        Decode.succeed Week

                    "month" ->
                        Decode.succeed Month

                    "year" ->
                        Decode.succeed Year

                    _ ->
                        Decode.fail ("Unknown timeframe: " ++ tf)
            )


encodePartitionParams : PartitionParams -> Value
encodePartitionParams params =
    Encode.object
        [ ( "colorMap", dictEncoder Encode.string params.colorMap )
        , ( "data", params.data )
        , ( "shape", encodePartitionShape params.shape )
        , ( "width", Encode.int params.width )
        , ( "height", Encode.int params.height )
        ]


encodePartitionDatum : PartitionDatum -> Value
encodePartitionDatum (Datum datum) =
    Encode.object
        [ ( "name", Encode.string datum.name )
        , ( "size"
          , case datum.size of
                Just s ->
                    Encode.int s

                Nothing ->
                    Encode.null
          )
        , ( "children"
          , case datum.children of
                Just c ->
                    encodePartitionData c

                Nothing ->
                    Encode.array Array.empty
          )
        , ( "others"
          , case datum.others of
                Just d ->
                    dictEncoder Encode.string d

                Nothing ->
                    Encode.object []
          )
        ]


encodePartitionData : List PartitionDatum -> Value
encodePartitionData data =
    Encode.list (List.map encodePartitionDatum data)


encodePartitionShape : PartitionShape -> Value
encodePartitionShape shape =
    Encode.string
        (case shape of
            Rectangle ->
                "rect"

            Arc ->
                "path"
        )


encodeTimeFrame : TimeFrame -> Value
encodeTimeFrame name =
    Encode.string
        (case name of
            Raw ->
                "raw"

            Day ->
                "day"

            Week ->
                "week"

            Month ->
                "month"

            Year ->
                "year"
        )



-- Utils


dictEncoder : (a -> Value) -> Dict String a -> Value
dictEncoder enc dict =
    Dict.toList dict
        |> List.map (\( k, v ) -> ( k, enc v ))
        |> Encode.object



-- Port data


type alias OutsideData =
    { tag : String
    , data : Encode.Value
    }



-- request params for filtering analytics data


type TimeFrame
    = Raw
    | Day
    | Week
    | Month
    | Year


type alias AnalyticsRequestParams =
    { start : Int
    , end : Int
    , unit : Int
    , timeFrame : TimeFrame
    }
