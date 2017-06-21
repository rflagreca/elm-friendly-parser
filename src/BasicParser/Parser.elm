module BasicParser.Parser exposing
    ( BasicParser
    , init, start, withRules
    , ReturnType(..)
    , Rules, Operator )

{-| TODO

@docs BasicParser

@docs init
    , start
    , withRules

@docs ReturnType

@docs Rules
    , Operator

-}
import Parser exposing (..)

{-| TODO -}
type ReturnType = RString String | RList (List ReturnType) | RRule RuleName ReturnType

{-| TODO -}
type alias BasicParser = Parser.Parser ReturnType

{-| TODO -}
type alias Operator = Parser.Operator ReturnType
{-| TODO -}
type alias Rules = Parser.Rules ReturnType
type alias RulesList = Parser.RulesList ReturnType
type alias InputType = Parser.InputType ReturnType

type alias ParseResult = Parser.ParseResult ReturnType

{-| TODO -}
init : BasicParser
init =
    Parser.init adapter

{-| TODO -}
start : Operator -> BasicParser
start op =
    Parser.start op adapter

-- startWith : Operator -> BasicParser -> BasicParser
-- startWith = Parser.startWith

-- addStartRule : Operator -> BasicParser -> BasicParser
-- addStartRule = Parser.addStartRule

-- parse : BasicParser -> String -> ParseResult
-- parse = Parser.parse

{-| TODO -}
withRules : RulesList -> BasicParser
withRules rules = init |> Parser.withRules rules

adapter : InputType -> ReturnType
adapter input =
    case input of
        Parser.AValue str -> RString str
        Parser.AList list -> RList list
        Parser.ARule name value -> RRule name value
