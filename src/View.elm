module View exposing (view)

import Dict.RBTree as RBDict
import Dict.TTTree as TTDict
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, maxlength, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Shared exposing (..)
import Svg exposing (Svg, line, path, rect, svg, text_)
import Svg.Attributes exposing (d, fill, height, rx, ry, stroke, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TreeDiagram exposing (Tree, node, topToBottom)
import TreeDiagram.Svg exposing (draw)


view : Model -> Html Msg
view model =
    div
        [ class "rb" ]
        [ h1
            []
            [ text "VISUALIZE RED-BLACK-TREE"
            ]
        , div
            [ class "rb-form" ]
            [ alert_ model.status
            , input_ model.form
            , button_ InsertKey "INSERT"
            , button_ RemoveKey "REMOVE"
            ]
        , div
            [ class "rb-graph" ]
            [ drawRBT model.rbt
            , drawTTT model.ttt
            ]
        ]



-- ELEMENT


alert_ : Status -> Html msg
alert_ status =
    let
        layout : { lavel : String, icon : Html msg, content : String } -> Html msg
        layout data =
            div
                [ class <| "alert " ++ data.lavel ]
                [ div
                    [ class "icon" ]
                    [ data.icon ]
                , div
                    [ class "content" ]
                    [ text data.content ]
                ]
    in
    case status of
        Before ->
            div [ class "alert" ] []

        Success value ->
            layout { lavel = "success", icon = successIcon, content = value }

        Error value ->
            layout { lavel = "error", icon = errorIcon, content = value }


input_ : String -> Html Msg
input_ form =
    input
        [ onInput EnteredKey
        , value form
        , type_ "number"
        , maxlength 5
        , placeholder "Enter KEY"
        ]
        []


button_ : Msg -> String -> Html Msg
button_ msg value =
    button
        [ onClick msg ]
        [ text value ]



-- ICON


successIcon : Html msg
successIcon =
    svg
        [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "#00875a"
        ]
        [ path [ d "M12 2c5.523 0 10 4.477 10 10s-4.477 10-10 10S2 17.523 2 12 6.477 2 12 2Zm0 1.5a8.5 8.5 0 1 0 0 17 8.5 8.5 0 0 0 0-17Zm-1.524 10.416 4.971-5.423a.75.75 0 0 1 1.175.927l-.07.087-5.5 6a.75.75 0 0 1-.996.098l-.086-.075-2.5-2.5a.75.75 0 0 1 .976-1.133l.084.073 1.946 1.946 4.971-5.423-4.97 5.423Z" ] []
        ]


errorIcon : Html msg
errorIcon =
    svg
        [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "#df0b37"
        ]
        [ path [ d "m13.047 5.599 6.786 11.586A1.207 1.207 0 0 1 18.786 19H5.214a1.207 1.207 0 0 1-1.047-1.815l6.786-11.586a1.214 1.214 0 0 1 2.094 0Zm-1.165.87a.234.234 0 0 0-.085.085L5.419 17.442a.232.232 0 0 0 .203.35h12.756a.234.234 0 0 0 .203-.35L12.203 6.554a.236.236 0 0 0-.321-.084ZM12 15.5a.75.75 0 1 1 0 1.5.75.75 0 0 1 0-1.5Zm-.024-6.22c.325 0 .589.261.589.583v4.434a.586.586 0 0 1-.589.583.586.586 0 0 1-.588-.583V9.863c0-.322.264-.583.588-.583Z" ] []
        ]



-- DRAW Red-Black-Tree GRAPH


drawRBT : RBT -> Html msg
drawRBT rbt =
    div
        [ class "rbt" ]
        [ draw
            { orientation = topToBottom
            , levelHeight = 40
            , siblingDistance = 100
            , subtreeDistance = 80
            , padding = 40
            }
            drawRBNode
            drawRBEdge
            (visualizeRBT rbt)
        ]


type alias RBNode =
    { isLeaf : Bool
    , color : RBDict.NColor
    , content : String
    }


drawRBNode : RBNode -> Svg msg
drawRBNode node =
    if node.isLeaf then
        drawRBLeaf

    else
        let
            bg =
                case node.color of
                    RBDict.Red ->
                        "#ff0000"

                    RBDict.Black ->
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


drawRBLeaf : Svg msg
drawRBLeaf =
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


drawRBEdge : ( Float, Float ) -> Svg msg
drawRBEdge ( targetX, targetY ) =
    Svg.line
        [ x1 "0"
        , y1 "0"
        , x2 (String.fromFloat targetX)
        , y2 (String.fromFloat targetY)
        , stroke "black"
        ]
        []


visualizeRBT : RBT -> Tree RBNode
visualizeRBT rbt =
    case rbt of
        RBDict.RBNode_elm_builtin color key () lExpr rExpr ->
            node
                { isLeaf = False
                , color = color
                , content = String.fromInt key
                }
                [ visualizeRBT lExpr
                , visualizeRBT rExpr
                ]

        RBDict.RBEmpty_elm_builtin ->
            node
                { isLeaf = True
                , color = RBDict.Black
                , content = ""
                }
                []



-- DRAW Two-Three-Tree GRAPH


drawTTT : TTT -> Html msg
drawTTT ttt =
    div
        [ class "ttt" ]
        [ draw
            { orientation = topToBottom
            , levelHeight = 40
            , siblingDistance = 100
            , subtreeDistance = 80
            , padding = 40
            }
            drawTTNode
            drawTTEdge
            (visualizeTTT ttt)
        ]


type alias TTNode =
    { isLeaf : Bool
    , startX : Int
    , content : List String
    , textX : List String
    }


drawTTNode : TTNode -> Svg msg
drawTTNode node =
    if node.isLeaf then
        drawTTLeaf

    else
        let
            lineBg =
                if List.length node.content == 2 then
                    "#ffffff"

                else
                    "#000000"
        in
        Svg.g
            []
            (rect
                [ rx "15"
                , ry "15"
                , x <| String.fromInt node.startX
                , y "-15"
                , height "30"
                , width <| String.fromInt (node.startX * -2)
                , fill "#000000"
                ]
                []
                :: line
                    [ x1 "0"
                    , x2 "0"
                    , y1 "-12"
                    , y2 "12"
                    , stroke lineBg
                    ]
                    []
                :: List.map2
                    (\textX content ->
                        text_
                            [ x textX
                            , textAnchor "middle"
                            , transform "translate(0,5)"
                            , fill "#ffffff"
                            ]
                            [ text content ]
                    )
                    node.textX
                    node.content
            )


drawTTLeaf : Svg msg
drawTTLeaf =
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


drawTTEdge : ( Float, Float ) -> Svg msg
drawTTEdge ( targetX, targetY ) =
    Svg.line
        [ x1 "0"
        , y1 "0"
        , x2 (String.fromFloat targetX)
        , y2 (String.fromFloat targetY)
        , stroke "black"
        ]
        []


visualizeTTT : TTT -> Tree TTNode
visualizeTTT ttt =
    case ttt of
        TTDict.TTNode2 a ( k1, _ ) b ->
            node
                { isLeaf = False
                , startX = -25
                , content = [ String.fromInt k1 ]
                , textX = [ "0" ]
                }
                [ visualizeTTT a
                , visualizeTTT b
                ]

        TTDict.TTNode3 a ( k1, _ ) b ( k2, _ ) c ->
            node
                { isLeaf = False
                , startX = -50
                , content = [ String.fromInt k1, String.fromInt k2 ]
                , textX = [ "-25", "25" ]
                }
                [ visualizeTTT a
                , visualizeTTT b
                , visualizeTTT c
                ]

        TTDict.TTEmpty ->
            node
                { isLeaf = True
                , startX = 0
                , content = []
                , textX = []
                }
                []
