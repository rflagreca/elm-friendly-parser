module Parser exposing (..)

import Dict exposing (..)

import User exposing (..)
import Operator exposing (..)

type alias Parser o = {
    adapt: Adapter o,
    rules: Rules o,
    startRule: String
}

parse : Parser o -> String -> ParseResult o
parse parser input =
    let
        ctx = (initContext input)
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
    , startRule = "start"
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

getStartRule : Context o -> Maybe (Operator o)
getStartRule ctx =
    Dict.get ctx.startRule ctx.rules

setStartRule : RuleName -> Parser o -> Parser o
setStartRule name parser =
    { parser | startRule = name }

addRule : RuleName -> Operator o -> Parser o -> Parser o
addRule name op parser =
    { parser | rules = parser.rules |> Operator.addRule name op }

getRule : RuleName -> Parser o -> Maybe (Operator o)
getRule name parser =
    Dict.get name parser.rules

addRule : RuleName -> Operator o -> Rules o -> Rules o
addRule name op rules =
    rules |> Dict.insert name op
