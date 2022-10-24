module Main exposing (main)

import Browser
import Shared exposing (..)
import View exposing (view)
import Dict.RBTree as RBDict


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredKey form ->
            { model | form = form }

        InsertKey ->
            case insertOk model of
                Ok key ->
                    { form = ""
                    , status = Success ("KEY:" ++ String.fromInt key ++ " was inserted.")
                    , expr = RBDict.insert key () model.expr
                    }

                Err error ->
                    { model | status = Error error }

        RemoveKey ->
            case removeOk model of
                Ok key ->
                    { form = ""
                    , status = Success ("KEY:" ++ String.fromInt key ++ " was removed.")
                    , expr = RBDict.remove key model.expr
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

            else if RBDict.member x model.expr then
                Err ("KEY:" ++ String.fromInt x ++ " is already a member.")

            else
                Ok x

        Nothing ->
            Err "Unable to convert to Int."


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

            else if RBDict.member x model.expr then
                Ok x

            else
                Err ("KEY:" ++ String.fromInt x ++ " is not a member.")

        Nothing ->
            Err "Unable to convert to Int."
