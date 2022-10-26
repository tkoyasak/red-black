module Main exposing (main)

import Browser
import Dict.RBTree as RBDict
import Shared exposing (..)
import View exposing (view)


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
                    , rbt = RBDict.insert key () model.rbt
                    }

                Err error ->
                    { model | status = Error error }

        RemoveKey ->
            case removeOk model of
                Ok key ->
                    { form = ""
                    , status = Success ("KEY:" ++ String.fromInt key ++ " was removed.")
                    , rbt = RBDict.remove key model.rbt
                    }

                Err error ->
                    { model | status = Error error }


insertOk : Model -> Result String Key
insertOk model =
    case String.toInt (String.trim model.form) of
        Just x ->
            if x > 99 || x < 0 then
                Err "Please enter 0-99."

            else if RBDict.member x model.rbt then
                Err ("KEY:" ++ String.fromInt x ++ " is already a member.")

            else
                Ok x

        Nothing ->
            Err "Unable to convert to Int."


removeOk : Model -> Result String Key
removeOk model =
    case String.toInt (String.trim model.form) of
        Just x ->
            if x > 99 || x < 0 then
                Err "Please enter 0-99."

            else if RBDict.member x model.rbt then
                Ok x

            else
                Err ("KEY:" ++ String.fromInt x ++ " is not a member.")

        Nothing ->
            Err "Unable to convert to Int."
