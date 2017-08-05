module StringParser.Parser exposing
    ( Parser
    , init, withRules, withGrammar, use
    , ParseResult
    , ReturnType(..)
    , Grammar, Rules, Operator, Token
    )

{-| TODO

@docs StringParser

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
import Parser as P exposing (..)
import Grammar as G exposing (..)
import Operator as O exposing (..)
import ParseResult as PR exposing (..)
import Match as M exposing (..)

{-| TODO -}
type ReturnType = Chunk String | Chunks (List ReturnType) | InRule String ReturnType

{-| TODO -}
type alias Parser = P.Parser ReturnType

{-| TODO -}
type alias Operator = O.Operator ReturnType
{-| TODO -}
type alias Grammar = G.Grammar ReturnType
{-| TODO -}
type alias Rules = G.Rules ReturnType
{-| TODO -}
type alias Token = M.Token ReturnType

{-| TODO -}
type alias ParseResult = PR.MyParseResult ReturnType

type alias Adapter k = (ReturnType -> k)

init : Parser
init = P.init |> P.adaptWith adapter

{-| TODO -}
withRules : Rules -> Parser
withRules = P.withRules

{-| TODO -}
withGrammar : Grammar -> Parser
withGrammar = P.withGrammar

{-| TODO -}
use : Operator -> Parser
use = P.use

{-| TODO -}
andUse : Operator -> Parser -> Parser
andUse = P.andUse

setStartRule : RuleName -> Parser -> Parser
setStartRule = P.setStartRule

-- adaptWith : Adapter k -> Parser -> Parser
-- adaptWith userAdapter cfg =
--     P.adaptWith adapter cfg

adapter : Token -> ReturnType
adapter token =
    case token of
        M.NoLexem -> Chunk ""
        M.Lexem str -> Chunk str
        M.Tokens tokens -> Chunks (List.map adapter tokens)
        _ -> Chunk ""
