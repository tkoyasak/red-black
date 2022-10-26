module Shared exposing (..)

import Dict.RBTree as RBDict


type alias Model =
    { form : String
    , status : Status
    , rbt : RBT
    }


type alias RBT =
    RBDict.Dict Key ()


type alias Key =
    Int


type Status
    = Before
    | Success String
    | Error String


init : Model
init =
    { form = ""
    , status = Before
    , rbt = RBDict.empty
    }


type Msg
    = EnteredKey String
    | InsertKey
    | RemoveKey
