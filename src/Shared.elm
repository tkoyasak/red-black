module Shared exposing (..)

import XDict as Dict exposing (Dict)


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


type Msg
    = EnteredKey String
    | InsertKey
    | RemoveKey
