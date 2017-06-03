module BasicParser.Parser exposing (..)

import Parser exposing (..)

type ReturnType = RString String | RList (List ReturnType) | RRule RuleName ReturnType

type alias BasicParser = Parser.Parser ReturnType

type alias Operator = Operator.Operator ReturnType
type alias ParseResult = Operator.ParseResult ReturnType
type alias Rules = Operator.Rules ReturnType
type alias RulesList = Operator.RulesList ReturnType
type alias InputType = User.InputType ReturnType

start : Operator -> BasicParser
start op =
    Parser.start op adapter

-- startWith : Operator -> BasicParser -> BasicParser
-- startWith = Parser.startWith

-- addStartRule : Operator -> BasicParser -> BasicParser
-- addStartRule = Parser.addStartRule

parse : BasicParser -> String -> ParseResult
parse = Parser.parse

withRules : Rules -> BasicParser
withRules rules =
    Parser.withRules rules adapter

withListedRules : RulesList -> BasicParser
withListedRules rulesList =
    Parser.withListedRules rulesList adapter

adapter : InputType -> ReturnType
adapter input =
    case input of
        User.AValue str -> RString str
        User.AList list -> RList list
        User.ARule name value -> RRule name value
