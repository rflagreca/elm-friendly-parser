module Parser exposing
    ( Parser, Config, parse, configure
    , withRules, withGrammar
    , use, setStartRule, adaptWith, andUse
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
    String -> ParseResult o

-- type alias ConfiguredParser o =
--     Config o -> Parser o

configure : Config o -> Parser o
configure config =
    (\input -> input |> parseWith config)

withRules : Rules o -> Config o
withRules rules =
    withGrammar (fromRules rules)

withGrammar : Grammar o -> Config o
withGrammar grammar =
    ( grammar, "start", Nothing )

use : Operator o -> Config o
use startOp =
    withGrammar (Grammar.empty |> addRule "start" startOp)

andUse : Operator o -> Config o -> Config o
andUse startOp ( grammar, startRule, maybeAdapter ) =
    let
        config = withGrammar (grammar |> addRule "start" startOp)
                 |> setStartRule startRule
    in
        case maybeAdapter of
            Just adapter -> config |> adaptWith adapter
            Nothing -> config

setStartRule : RuleName -> Config o -> Config o
setStartRule startRule ( grammar, _, maybeAdapter ) =
    ( grammar, startRule, maybeAdapter )

adaptWith : Adapter o -> Config o -> Config o
adaptWith adapter ( grammar, startRule, _ ) =
    ( grammar, startRule, Just adapter )

suggestStartRule : Grammar o -> Maybe String -> String
suggestStartRule grammar maybeName =
    case maybeName of
        Just name -> name
        Nothing ->
            case grammar |> getRule "start" of
                Just _ -> "start"
                Nothing -> List.head (Dict.keys grammar)
                    |> Maybe.withDefault "<UNKNOWN>"

parse : String -> Parser o -> ParseResult o
parse input parser =
    parser input

parseWith : Config o -> String -> ParseResult o
parseWith = Execute.try

fromOperator : Operator o -> Parser o
fromOperator startOperator =
    (\input -> input |> Execute.tryOne startOperator)

-- parse : String -> Parser o -> ParseResult o
-- parse input parser =
--     parser input

-- UTILS

-- extractParseResult : OperatorResult o -> ParseResult o
-- extractParseResult opResult =
--     Tuple.first opResult

-- extractContext : OperatorResult o -> Context o
-- extractContext opResult =
--     Tuple.second opResult

-- opResultToMaybe : OperatorResult o -> ( Maybe o, Context o )
-- opResultToMaybe ( parseResult, ctx ) =
--     ( parseResultToMaybe parseResult, ctx )
