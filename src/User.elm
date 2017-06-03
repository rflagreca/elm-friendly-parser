module User exposing (..)

type InputType o =
      AValue String
    | AList (List o)
    | ARule RuleName o

type alias Adapter o = (InputType o -> o)

type alias RuleName = String
