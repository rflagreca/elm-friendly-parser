module StringParser.Parser exposing
    ( Parser
    , init, start, withRules
    , ParseResult
    , ReturnType(..)
    , Grammar, Rules, Operator, Token
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
type ReturnType = Chunk String | Chunks (List String) | In_Rule String ReturnType

{-| TODO -}
type alias Parser = Parser.Parser ReturnType

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
init : Parser
init = Parser.init

{-| TODO -}
start : Operator -> Parser
start = Parser.start

-- startWith : Operator -> BasicParser -> BasicParser
-- startWith = Parser.startWith

-- addStartRule : Operator -> BasicParser -> BasicParser
-- addStartRule = Parser.addStartRule

-- parse : BasicParser -> String -> ParseResult
-- parse = Parser.parse

{-| TODO -}
withRules : Rules -> Parser
withRules rules = init |> Parser.withRules rules

{-
adapter : Token -> ReturnType
adapter token =
    case token of
        Match.NoLexem -> Chunk ""
        Match.Lexem str -> Chunk str
        Match.Tokens tokens -> Chunks (List.map adapter tokens)
        _ -> Chunk ""
-}
