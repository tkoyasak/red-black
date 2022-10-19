module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, li, p, text, ul)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg, rect, text_)
import Svg.Attributes exposing (fill, height, rx, ry, stroke, textAnchor, transform, width, x, x1, x2, y, y1, y2)
import TreeDiagram exposing (Tree, node, topToBottom)
import TreeDiagram.Svg exposing (draw)
import XDict as Dict exposing (Dict)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- INIT


type alias Model =
    { form : Form
    , problems : List Problem
    , expr : RBT
    }


type alias RBT =
    Dict Key ()


type alias Key =
    Int


type alias Form =
    String


type alias Problem =
    String


init : Model
init =
    { form = ""
    , problems = []
    , expr = Dict.empty
    }



-- UPDATE


type Msg
    = EnteredKey String
    | InsertKey
    | RemoveKey


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredKey form ->
            { model | form = form }

        InsertKey ->
            case insertOk model of
                Ok key ->
                    { form = ""
                    , problems = []
                    , expr = Dict.insert key () model.expr
                    }

                Err problems ->
                    { model | problems = problems }

        RemoveKey ->
            case removeOk model of
                Ok key ->
                    { form = ""
                    , problems = []
                    , expr = Dict.remove key model.expr
                    }

                Err problems ->
                    { model | problems = problems }


insertOk : Model -> Result (List Problem) Key
insertOk model =
    let
        trimmedForm =
            String.trim model.form
    in
    case String.toInt trimmedForm of
        Just x ->
            if Dict.member x model.expr then
                Err [ "That's already a member. [" ++ trimmedForm ++ "]" ]

            else if x > 999 || x < 0 then
                Err [ "Please input 0-999." ]

            else
                Ok x

        Nothing ->
            Err [ "Unable to convert \"" ++ trimmedForm ++ "\" to Int." ]


removeOk : Model -> Result (List Problem) Key
removeOk model =
    let
        trimmedForm =
            String.trim model.form
    in
    case String.toInt trimmedForm of
        Just x ->
            if Dict.member x model.expr then
                Ok x

            else
                Err [ "There is no such member. [" ++ trimmedForm ++ "]" ]

        Nothing ->
            Err [ "Unable to convert \"" ++ trimmedForm ++ "\" to Int." ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "" ]
        [ h1
            []
            [ text "visualize-red-black" ]
        , button
            [ onClick InsertKey ]
            [ text "Insert" ]
        , button
            [ onClick RemoveKey ]
            [ text "Remove" ]
        , input
            [ value model.form, onInput EnteredKey, placeholder "key" ]
            []
        , ul []
            (List.map
                (\problem ->
                    li [] [ text problem ]
                )
                model.problems
            )
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
            [ rx "15"
            , ry "15"
            , x "-15"
            , y "-15"
            , height "30"
            , width "30"
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
