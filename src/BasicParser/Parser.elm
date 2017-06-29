module BasicParser.Parser exposing
    ( BasicParser
    , init, start, withRules
    , ParseResult
    , ReturnType(..)
    , Grammar, Rules, Operator, InputType
    )

{-| TODO

@docs BasicParser

# Initialization

@docs init
    , start
    , withRules

# Parse Result

@docs ParseResult

# Return Type

@docs ReturnType

# Extensions

@docs Grammar
    , Rules
    , Operator
    , InputType

-}
import Parser exposing (..)
import Operator exposing (..)
import ParseResult exposing (..)
import Match exposing (..)

{-| TODO -}
type ReturnType = RString String | RList (List ReturnType) | RRule RuleName ReturnType

{-| TODO -}
type alias BasicParser = Parser.Parser ReturnType

{-| TODO -}
type alias Operator = Operator.Operator ReturnType
{-| TODO -}
type alias Grammar = Operator.Grammar ReturnType
{-| TODO -}
type alias Rules = Operator.Rules ReturnType
{-| TODO -}
type alias Token = Match.Token ReturnType

{-| TODO -}
type alias ParseResult = ParseResult.ParseResult ReturnType

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
withRules : Rules -> BasicParser
withRules rules = init |> Parser.withRules rules

adapter : InputType -> ReturnType
adapter input =
    case input of
        Adapter.AValue str -> RString str
        Adapter.AList list -> RList list
        Adapter.ARule name value -> RRule name value
