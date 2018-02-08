module App.Request exposing (..)

import Http
import Dict
import Json.Decode as Decode exposing (list)
import RemoteData exposing (WebData)
import App.Data exposing (..)


-- vanilla request
{- getDataPoints : String -> Cmd Msg
   getDataPoints =
       let
           url =
               "../Assets/json/dataPoints.json"

           dataPointList =
               Result Http.Error (List DataPoint)
       in
           Http.send dataPointList (Http.get url DataPoint.decoder)
-}


getDataPoints : (WebData (List DataPoint) -> msg) -> String -> Cmd msg
getDataPoints toMsg api =
    Http.get (api ++ "/data.json") (Decode.list dataPointDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map toMsg


getDataPointAnalyses : (WebData PartitionDatum -> msg) -> String -> Cmd msg
getDataPointAnalyses toMsg api =
    Http.get (api ++ "/dataAnalyses.json") (partitionDatumDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map toMsg


filterAnalysesByTime : List PartitionDatum -> AnalyticsRequestParams -> List PartitionDatum
filterAnalysesByTime data params =
    List.filter
        (\(App.Data.Datum d) ->
            case d.others of
                Just others ->
                    let
                        unit =
                            case Dict.get "unit" others of
                                Just u ->
                                    case String.toInt u of
                                        Ok uInt ->
                                            uInt

                                        Err _ ->
                                            0

                                Nothing ->
                                    0

                        start =
                            case Dict.get "start" others of
                                Just s ->
                                    case String.toInt s of
                                        Ok sInt ->
                                            sInt

                                        Err _ ->
                                            0

                                Nothing ->
                                    0

                        end =
                            case Dict.get "end" others of
                                Just e ->
                                    case String.toInt e of
                                        Ok eInt ->
                                            eInt

                                        Err _ ->
                                            0

                                Nothing ->
                                    0

                        nameAsTimeFrame =
                            case Decode.decodeString timeFrameDecoder (toString d.name) of
                                Ok tf ->
                                    tf

                                Err err ->
                                    Raw
                    in
                        (nameAsTimeFrame == params.timeFrame) && (unit == params.unit) && (start >= params.start) && (end <= params.end)

                Nothing ->
                    False
        )
        data
