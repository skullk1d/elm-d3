module App.View.PartitionMap exposing (..)

import Element exposing (..)
import Style exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import App.Data exposing (Partition, PartitionNode(Rect, Path), PartitionParams)


-- STYLE


type Styles
    = None


styles : List (Style Styles variation)
styles =
    [ Style.style None []
    ]



-- VIEW


view : Partition -> Int -> Int -> Element Styles variation msg
view partition w h =
    let
        nodes =
            partition.nodes

        viewBox =
            partition.viewBox
    in
        column None
            []
            [ el None
                []
                (if List.isEmpty nodes then
                    empty
                 else
                    svg
                        [ width (toString w), height (toString h), Svg.Attributes.viewBox viewBox ]
                        (List.map
                            (\node ->
                                case node of
                                    Rect rectNode ->
                                        Svg.rect
                                            [ Svg.Attributes.x rectNode.x
                                            , Svg.Attributes.y rectNode.y
                                            , Svg.Attributes.width rectNode.width
                                            , Svg.Attributes.height rectNode.height
                                            , Svg.Attributes.style rectNode.style
                                            ]
                                            []

                                    Path pathNode ->
                                        Svg.path
                                            [ Svg.Attributes.d pathNode.d
                                            , Svg.Attributes.style pathNode.style
                                            , Svg.Attributes.transform pathNode.transform
                                            ]
                                            []
                            )
                            nodes
                        )
                        |> html
                )
            ]



{- svg
   [ width "120", height "120", viewBox "0 0 120 120" ]
   [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]
-}
