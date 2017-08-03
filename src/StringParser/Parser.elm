module StringParser.Parser exposing
    ( Parser
    , Config
    , configure, withRules, use
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
type ReturnType = Chunk String | Chunks (List ReturnType) | InRule String ReturnType

{-| TODO -}
type alias Parser = Parser.Parser ReturnType
{-| TODO -}
type alias Config = Parser.Config ReturnType

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

type alias Adapter k = (ReturnType -> k)

{-| TODO -}
configure : Config -> Parser
configure = Parser.configure

{-| TODO -}
withRules : Rules -> Config
withRules = Parser.withRules

{-| TODO -}
withGramar : Grammar -> Config
withGramar = Parser.withGrammar

{-| TODO -}
use : Operator -> Config
use = Parser.use

{-| TODO -}
andUse : Operator -> Config -> Config
andUse = Parser.andUse

setStartRule : RuleName -> Config -> Config
setStartRule = Parser.setStartRule

-- adaptWith : Adapter k -> Config -> Config
-- adaptWith userAdapter cfg =
--     Parser.adaptWith adapter cfg

adapter : Token -> ReturnType
adapter token =
    case token of
        Match.NoLexem -> Chunk ""
        Match.Lexem str -> Chunk str
        Match.Tokens tokens -> Chunks (List.map adapter tokens)
        _ -> Chunk ""
