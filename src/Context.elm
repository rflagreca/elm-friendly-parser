module Context exposing (..)

import Dict exposing (..)

type alias Values o = Dict String o

initContext : String -> Context o
initContext input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , values = noValues
    }

type alias Context o =
    { input: String
    , inputLength: Int
    , position: Int
    , values: Values o
}

noValues : Values v
noValues = Dict.empty
