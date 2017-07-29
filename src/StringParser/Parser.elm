module StringParser.Parser exposing
    ( Parser
    , init, use
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
import Grammar exposing (..)
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
type alias Grammar = Grammar.Grammar ReturnType
{-| TODO -}
type alias Rules = Grammar.Rules ReturnType
{-| TODO -}
type alias Token = Match.Token ReturnType

{-| TODO -}
type alias ParseResult = ParseResult.ParseResult ReturnType

{-| TODO -}
init : Rules -> Parser
init = Parser.init

{-| TODO -}
use : Operator -> Parser
use = Parser.use

-- startWith : Operator -> BasicParser -> BasicParser
-- startWith = Parser.startWith

-- addStartRule : Operator -> BasicParser -> BasicParser
-- addStartRule = Parser.addStartRule

-- parse : BasicParser -> String -> ParseResult
-- parse = Parser.parse

{-
adapter : Token -> ReturnType
adapter token =
    case token of
        Match.NoLexem -> Chunk ""
        Match.Lexem str -> Chunk str
        Match.Tokens tokens -> Chunks (List.map adapter tokens)
        _ -> Chunk ""
-}
