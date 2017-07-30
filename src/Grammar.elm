module Grammar exposing
    ( Grammar
    , Rules
    , noRules
    , empty
    , getRule
    )

 {- TODO: rename module to Grammar -}

import Dict exposing (Dict)

import Operator exposing
    ( Operator, Rule, RuleName
    )

type alias Grammar o = Dict RuleName (Operator o)
type alias Rules o = List (Rule o)


-- UTILS

noRules : Rules o
noRules = []

empty : Grammar o
empty = Dict.empty

getRule : RuleName -> Grammar o -> Maybe (Operator o)
getRule name grammar =
    Dict.get name grammar

