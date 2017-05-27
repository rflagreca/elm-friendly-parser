module User exposing (..)

import Dict exposing (Dict)

type InputType o =
      AValue String
    | AList (List o)
    | ARule RuleName o

type alias Values o = Dict String o

type alias Adapter o = (InputType o -> o)

type alias RuleName = String
