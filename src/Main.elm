module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, p, text)
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
    { form : String
    , status : Status String
    , expr : RBT
    }


type alias RBT =
    Dict Key ()


type alias Key =
    Int


type Status value
    = Before
    | Complete value
    | Error value


init : Model
init =
    { form = ""
    , status = Before
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
                    , status = Complete ("Key:" ++ String.fromInt key ++ " was inserted.")
                    , expr = Dict.insert key () model.expr
                    }

                Err error ->
                    { model | status = Error error }

        RemoveKey ->
            case removeOk model of
                Ok key ->
                    { form = ""
                    , status = Complete ("Key:" ++ String.fromInt key ++ " was removed.")
                    , expr = Dict.remove key model.expr
                    }

                Err error ->
                    { model | status = Error error }


insertOk : Model -> Result String Key
insertOk model =
    let
        trimmedForm =
            String.trim model.form
    in
    case String.toInt trimmedForm of
        Just x ->
            if x > 99 || x < 0 then
                Err "Please input 0-99."

            else if Dict.member x model.expr then
                Err ("Key:" ++ String.fromInt x ++ " is already a member.")

            else
                Ok x

        Nothing ->
            Err ("Unable to convert \"" ++ trimmedForm ++ "\" to Int.")


removeOk : Model -> Result String Key
removeOk model =
    let
        trimmedForm =
            String.trim model.form
    in
    case String.toInt trimmedForm of
        Just x ->
            if x > 99 || x < 0 then
                Err "Please input 0-99."

            else if Dict.member x model.expr then
                Ok x

            else
                Err ("Key:" ++ String.fromInt x ++ "is not a member.")

        Nothing ->
            Err ("Unable to convert \"" ++ trimmedForm ++ "\" to Int.")



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "" ]
        [ h1
            []
            [ text "Visualize Red-Black-Tree" ]
        , input
            [ value model.form, onInput EnteredKey, placeholder "Key" ]
            []
        , button
            [ onClick InsertKey ]
            [ text "INSERT" ]
        , button
            [ onClick RemoveKey ]
            [ text "REMOVE" ]
        , p
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
