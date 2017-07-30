module Parser exposing
    ( Parser, parse
    , withRules, withGrammar
    , use, setStartRule, adaptWith
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
    )
import Execute exposing
    ( Context
    , execute
    , getCurrentChar
    , toResult
    , failByEndOfInput
    )
import State as State exposing
    ( State
    , Position
    , findPosition
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

withRules : Rules o -> Config o
withRules rules =
    withGrammar (fromRules rules)

withGrammar : Grammar o -> Config o
withGrammar grammar =
    ( grammar, "start", Nothing )

use : Operator o -> Config o
use startOp =
    withGrammar (Grammar.empty |> addRule "start" startOp)

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

parse : Config o -> String -> ParseResult o
parse ( grammar, startRule, maybeAdapter ) input =
    let

        state = (State.init input)
        context =
            { adapter = maybeAdapter
            , grammar = grammar
            , state = state
            }
    in
        case Dict.get startRule grammar of
            Just startOperator ->
                -- TODO: extractParseResult (execCall parser.startRule context)
                let
                    ( opResult, lastState ) = (execute startOperator context)
                in
                    case toResult opResult of
                        Ok success ->
                            if lastState.position == (String.length input) then
                                Matched success
                            else
                                let
                                    ( reason, position ) = lastState |> failByEndOfInput
                                in
                                    Failed reason position
                        Err reason -> Failed reason (findPosition lastState)
            Nothing -> Failed NoStartRule (0, 0)

-- parse : String -> Parser o -> ParseResult o
-- parse input parser =
--     parser input

addRule : RuleName -> Operator o -> Grammar o -> Grammar o
addRule name op grammar =
    ( grammar |> Dict.insert name op )

getRule : RuleName -> Grammar o -> Maybe (Operator o)
getRule name grammar =
    Dict.get name grammar

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

parseResultToMaybe : ParseResult o -> Maybe (Token o)
parseResultToMaybe result =
    case result of
        Matched v -> Just v
        Failed _ _ -> Nothing

parseResultToResult : ParseResult o -> Result (FailureReason o) (Token o)
parseResultToResult result =
    case result of
        Matched v -> Ok v
        Failed f _ -> Err f
