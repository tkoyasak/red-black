module Pages.Home_ exposing (Model, Msg, page)

import Browser.Dom exposing (Error(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page exposing (Page)
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
    , dict : Dict Int String
    }


type alias Form =
    { key : String
    , value : String
    }


type alias Pair =
    { key : Int
    , value : String
    }


type Problem
    = InvalidEntry String
    | NotFoundMember String


init : Model
init =
    { form = { key = "", value = "" }
    , problems = []
    , dict = Dict.empty
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
                    , dict = Dict.insert pair.key pair.value model.dict
                    }

                Err problems ->
                    { model | problems = problems }

        RemovePair ->
            case validate model.form of
                Ok pair ->
                    if Dict.member pair.key model.dict then
                        { form = { key = "", value = "" }
                        , problems = []
                        , dict = Dict.remove pair.key model.dict
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
                [ text <| "Dict size : " ++ String.fromInt (Dict.size model.dict) ]
            ]
        ]
    }
