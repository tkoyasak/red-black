module View exposing (..)

import Html exposing (Html, button, div, h1, input, p, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Shared exposing (..)
import Svg exposing (Svg, path, rect, svg, text_)
import Svg.Attributes exposing (d, fill, height, rx, ry, stroke, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TreeDiagram exposing (Tree, node, topToBottom)
import TreeDiagram.Svg exposing (draw)
import XDict as Dict


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Visualize Red-Black-Tree" ]
        , div
            []
            [ div
                []
                ((\status ->
                    case status of
                        Before ->
                            []

                        Complete value ->
                            [ text value ]

                        Error value ->
                            [ text value ]
                 )
                    model.status
                )
            , input
                [ value model.form, onInput EnteredKey, placeholder "Key" ]
                []
            ]
        , div
            []
            [ button
                [ onClick InsertKey ]
                [ text "INSERT" ]
            , button
                [ onClick RemoveKey ]
                [ text "REMOVE" ]
            ]
        , p
            []
            [ text <| "size : " ++ String.fromInt (Dict.size model.expr) ]
        , drawRBT model.expr
        ]


drawRBT : RBT -> Html msg
drawRBT expr =
    draw
        { orientation = topToBottom
        , levelHeight = 40
        , siblingDistance = 100
        , subtreeDistance = 80
        , padding = 40
        }
        drawNode
        drawEdge
        (visualizeRBT expr)


type alias Node =
    { isLeaf : Bool
    , color : Dict.NColor
    , content : String
    }


drawNode : Node -> Svg msg
drawNode node =
    if node.isLeaf then
        drawLeaf

    else
        let
            bg =
                case node.color of
                    Dict.Red ->
                        "#ff0000"

                    Dict.Black ->
                        "#000000"
        in
        Svg.g
            []
            [ rect
                [ rx "15"
                , ry "15"
                , x "-25"
                , y "-15"
                , height "30"
                , width "50"
                , fill bg
                ]
                []
            , text_
                [ textAnchor "middle"
                , transform "translate(0,5)"
                , fill "#ffffff"
                ]
                [ text node.content ]
            ]


drawLeaf : Svg msg
drawLeaf =
    Svg.g
        []
        [ rect
            [ rx "10"
            , ry "10"
            , x "-10"
            , y "-10"
            , height "20"
            , width "20"
            , fill "#cccccc"
            ]
            []
        ]


drawEdge : ( Float, Float ) -> Svg msg
drawEdge ( targetX, targetY ) =
    Svg.line
        [ x1 "0"
        , y1 "0"
        , x2 (String.fromFloat targetX)
        , y2 (String.fromFloat targetY)
        , stroke "black"
        ]
        []


visualizeRBT : RBT -> Tree Node
visualizeRBT expr =
    case expr of
        Dict.RBNode_elm_builtin color key () lExpr rExpr ->
            node
                { isLeaf = False
                , color = color
                , content = String.fromInt key
                }
                [ visualizeRBT lExpr
                , visualizeRBT rExpr
                ]

        Dict.RBEmpty_elm_builtin ->
            node
                { isLeaf = True
                , color = Dict.Black
                , content = ""
                }
                []
