module BasicParser exposing (..)

import Parser exposing (..)

import List exposing (..)
import String exposing (..)

type ReturnType = RString String | RList (List ReturnType)

type alias BasicParser = Parser.Parser ReturnType

type alias Operator = Parser.Operator ReturnType
type alias ParseResult = Parser.ParseResult ReturnType
type alias Rules = Parser.Rules ReturnType
type alias RulesList = Parser.RulesList ReturnType
type alias InputType = Parser.InputType ReturnType

start : Operator -> BasicParser
start op =
    Parser.start op adapter

startWith : Operator -> BasicParser -> BasicParser
startWith = Parser.startWith

addStartRule : Operator -> BasicParser -> BasicParser
addStartRule = Parser.addStartRule

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
        Parser.AValue str -> RString str
        Parser.AList list -> RList list

toString : ReturnType -> String
toString value =
    case value of
        RString str -> str
        RList list -> String.join "," (List.map toString list)
