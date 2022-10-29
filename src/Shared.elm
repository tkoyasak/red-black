module Shared exposing (..)

import Dict.RBTree as RBDict
import Dict.TTTree as TTDict


type alias Model =
    { form : String
    , status : Status
    , rbt : RBT
    , ttt : TTT
    }


type alias RBT =
    RBDict.Dict Key ()


type alias TTT =
    TTDict.Dict Key ()


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
    , ttt = TTDict.empty
    }


type Msg
    = EnteredKey String
    | InsertKey
    | RemoveKey
