module Parser exposing
    ( Parser, parse
    , init, withRules, withGrammar, use
    , setStartRule, adaptWith, andUse
    )

import Dict exposing (..)

import Operator exposing
    ( RuleName
    )
import Grammar exposing
    ( Grammar
    , Rules
    , empty
    , noRules
    , fromRules
    , getRule
    , addRule
    )
import Execute exposing
    ( Context
    , execute
    , try
    , tryOne
    )
import ParseResult exposing
    ( ParseResult(..)
    , FailureReason(..)
    , Expectation(..)
    , Sample(..)
    )
import Operator exposing (Operator, RuleName)
import Match exposing (Adapter, Token)

type alias Config o =
    (Grammar o, RuleName, Maybe (Adapter o))

type alias Parser o =
    { grammar: Grammar o
    , maybeStartRule: Maybe RuleName
    , maybeAdapter: Maybe (Adapter o)
    }

init : Parser o
init =
    { grammar = Grammar.empty
    , maybeStartRule = Nothing
    , maybeAdapter = Nothing
    }

withRules : Rules o -> Parser o
withRules rules =
    withGrammar (fromRules rules)

withGrammar : Grammar o -> Parser o
withGrammar grammar =
    { init | grammar = grammar }

use : Operator o -> Parser o
use startOp =
    withGrammar (Grammar.empty |> addRule "start" startOp)

andUse : Operator o -> Parser o -> Parser o
andUse startOp parser =
    { parser
    | grammar = (Grammar.empty |> addRule "start" startOp)
    , maybeStartRule = Nothing
    }

setStartRule : RuleName -> Parser o -> Parser o
setStartRule startRule parser =
    { parser | maybeStartRule = Just startRule }

adaptWith : Adapter o -> Parser o -> Parser o
adaptWith adapter parser =
    { parser | maybeAdapter = Just adapter }

suggestStartRule : Maybe String -> Grammar o -> String
suggestStartRule maybeName grammar =
    case maybeName of
        Just name -> name
        Nothing ->
            case grammar |> getRule "start" of
                Just _ -> "start"
                Nothing -> List.head (Dict.keys grammar)
                    |> Maybe.withDefault "<UNKNOWN>"

parse : String -> Parser o -> ParseResult o
parse input parser =
    let
        startRule = parser.grammar |> suggestStartRule parser.maybeStartRule
    in
        Execute.try (parser.grammar, startRule, parser.maybeAdapter) input
