module BasicParser exposing (..)

import User exposing (..)
import Operator exposing (..)
import Parser exposing (..)

import List exposing (..)
import String exposing (..)

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
        User.AValue str -> RString str
        User.AList list -> RList list
        User.ARule name value -> RRule name value

toString : ReturnType -> String
toString value =
    case value of
        RString str -> str
        RList list -> String.join "," (List.map toString list)
        RRule name value -> name ++ ": " ++ (toString value)
