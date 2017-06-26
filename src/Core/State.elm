module Core.State exposing
    ( Values
    , State
    , init
    )

import Dict exposing (Dict)

type alias Values o = Dict String o

noValues : Values v
noValues = Dict.empty

-- FIXME: it should be possible to get ( Line, Position ),
-- and also Previous Position in state
type alias State o =
    { input: String
    , inputLength: Int
    , position: Int
    , values: Values o
}

init : String -> State o
init input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , values = noValues
    }
