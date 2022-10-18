module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom exposing (Error(..))
import Html exposing (Html, button, div, h1, input, li, p, text, ul)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Page exposing (Page)
import Svg exposing (Svg, rect, text_)
import Svg.Attributes exposing (fill, height, rx, ry, stroke, textAnchor, transform, width, x, x1, x2, y, y1, y2)
import TreeDiagram exposing (Tree, node, topToBottom)
import TreeDiagram.Svg exposing (draw)
import View exposing (View)
import X.Dict as Dict exposing (Dict)


page : Page Model Msg
page =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { form : Form
    , problems : List Problem
    , expr : RBT
    }


type alias RBT =
    Dict Key Value


type alias Key =
    Int


type alias Value =
    String


type alias Form =
    { key : String
    , value : String
    }


type alias Pair =
    { key : Key
    , value : Value
    }


type Problem
    = InvalidEntry String
    | NotFoundMember String


init : Model
init =
    { form = { key = "", value = "" }
    , problems = []
    , expr = Dict.empty
    }



-- UPDATE


type Msg
    = EnteredKey String
    | EnteredValue String
    | InsertPair
    | RemovePair


update : Msg -> Model -> Model
update msg ({ form } as model) =
    let
        setForm : Form -> Model
        setForm a =
            { model | form = a }
    in
    case msg of
        EnteredKey key ->
            setForm { form | key = key }

        EnteredValue value ->
            setForm { form | value = value }

        InsertPair ->
            case validate model.form of
                Ok pair ->
                    { form = { key = "", value = "" }
                    , problems = []
                    , expr = Dict.insert pair.key pair.value model.expr
                    }

                Err problems ->
                    { model | problems = problems }

        RemovePair ->
            case validate model.form of
                Ok pair ->
                    if Dict.member pair.key model.expr then
                        { form = { key = "", value = "" }
                        , problems = []
                        , expr = Dict.remove pair.key model.expr
                        }

                    else
                        { model | problems = [ NotFoundMember "There is no such member in Dict." ] }

                Err problems ->
                    { model | problems = problems }


validate : Form -> Result (List Problem) Pair
validate form =
    let
        trimmedForm =
            { key = String.trim form.key
            , value = String.trim form.value
            }
    in
    case String.toInt trimmedForm.key of
        Just x ->
            Ok { key = x, value = trimmedForm.value }

        Nothing ->
            Err [ InvalidEntry "Unable to convert String to Int." ]



-- VIEW


view : Model -> View Msg
view model =
    { title = "visualize-red-black"
    , body =
        [ div
            [ class "" ]
            [ h1
                []
                [ text "visualize-red-black" ]
            , button
                [ onClick InsertPair ]
                [ text "Insert" ]
            , button
                [ onClick RemovePair ]
                [ text "Remove" ]
            , input
                [ value model.form.key, onInput EnteredKey, placeholder "key" ]
                []
            , input
                [ value model.form.value, onInput EnteredValue, placeholder "value" ]
                []
            , ul []
                (List.map
                    (\problem ->
                        case problem of
                            InvalidEntry str ->
                                li [] [ text str ]

                            NotFoundMember str ->
                                li [] [ text str ]
                    )
                    model.problems
                )
            , p
                []
                [ text <| "Dict size : " ++ String.fromInt (Dict.size model.expr) ]
            , drawRBT model.expr
            ]
        ]
    }


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
    { color : Dict.NColor
    , pair : Pair
    }


drawNode : Node -> Svg msg
drawNode node =
    let
        content =
            String.fromInt node.pair.key

        bg =
            case node.color of
                Dict.Black ->
                    "#000000"

                Dict.Red ->
                    "#ff0000"
    in
    Svg.g
        []
        [ rect
            [ rx "15"
            , ry "15"
            , x "-40"
            , y "-15"
            , height "30"
            , width "80"
            , stroke "black"
            , fill bg
            ]
            []
        , text_
            [ textAnchor "middle"
            , transform "translate(0,5)"
            , fill "#ffffff"
            ]
            [ text content ]
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
        Dict.RBNode_elm_builtin color key value lExpr rExpr ->
            node
                { color = color
                , pair = { key = key, value = value }
                }
                [ visualizeRBT lExpr
                , visualizeRBT rExpr
                ]

        Dict.RBEmpty_elm_builtin ->
            node
                { color = Dict.Black
                , pair = { key = 0, value = "" }
                }
                []
