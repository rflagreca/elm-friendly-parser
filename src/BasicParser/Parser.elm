module BasicParser.Parser exposing (..)

import Parser exposing (..)

type ReturnType = RString String | RList (List ReturnType) | RRule RuleName ReturnType

type alias BasicParser = Parser.Parser ReturnType

type alias Operator = Parser.Operator ReturnType
type alias ParseResult = Parser.ParseResult ReturnType
type alias Rules = Parser.Rules ReturnType
type alias RulesList = Parser.RulesList ReturnType
type alias InputType = Parser.InputType ReturnType

init : BasicParser
init =
    Parser.init adapter

start : Operator -> BasicParser
start op =
    Parser.start op adapter

-- startWith : Operator -> BasicParser -> BasicParser
-- startWith = Parser.startWith

-- addStartRule : Operator -> BasicParser -> BasicParser
-- addStartRule = Parser.addStartRule

-- parse : BasicParser -> String -> ParseResult
-- parse = Parser.parse

withRules : RulesList -> BasicParser
withRules rules = init |> Parser.withRules rules

adapter : InputType -> ReturnType
adapter input =
    case input of
        Parser.AValue str -> RString str
        Parser.AList list -> RList list
        Parser.ARule name value -> RRule name value
