module Parser exposing (..)

import Dict exposing (..)

import User exposing (..)
import Operator exposing (..)

type alias Parser o = {
    adapt: Adapter o,
    rules: Rules o
}

parse : Parser o -> String -> ParseResult o
parse parser input =
    let
        ctx = (initContext parser input)
    in
        case getStartRule parser of
            Just startOperator ->
                extractParseResult (execute startOperator ctx)
            Nothing -> Failed NoStartRule

noRules : Rules o
noRules = Dict.empty

withRules : Rules o -> Adapter o -> Parser o
withRules rules adapter =
    { adapt = adapter
    , rules = rules
    }

-- TODO: withRules should accept RulesList instead

withListedRules : RulesList o -> Adapter o -> Parser o
withListedRules rulesList adapter =
    withRules (Dict.fromList rulesList) adapter

start : Operator o -> Adapter o -> Parser o
start op adapter =
    let
        justStartRule = (noRules |> Operator.addRule "start" op)
    in
        withRules justStartRule adapter

startWith : Operator o -> Parser o -> Parser o
startWith op parser =
    parser |> addRule "start" op

addStartRule : Operator o -> Parser o -> Parser o
addStartRule = startWith

initContext : Parser o -> String -> Context o
initContext parser input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , rules = parser.rules
    , values = noValues
    , adapt = parser.adapt
    }

getStartRule : Parser o -> Maybe (Operator o)
getStartRule parser =
    Dict.get "start" parser.rules

addRule : RuleName -> Operator o -> Parser o -> Parser o
addRule name op parser =
    { parser | rules = parser.rules |> Operator.addRule name op }

getRule : RuleName -> Parser o -> Maybe (Operator o)
getRule name parser =
    Dict.get name parser.rules

noValues : Values v
noValues = Dict.empty
