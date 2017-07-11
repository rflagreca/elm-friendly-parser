module Parser exposing
    ( Parser, init, start, startWith, parse
    -- , Position, ParseResult(..), FailureReason(..), Expectation(..), Sample(..)
    , withRules, setStartRule, getRule --, noRules, RuleName, Rules, RulesList
    -- , ActionResult(..), PrefixActionResult(..), UserCode, UserPrefixCode
    -- , InputType(..)
    -- , Adapter
    -- , Operator(..), State
    )

import Dict exposing (..)

import Operator exposing
    ( Operator
    , Context
    , execute
    , Rules
    , RuleName
    , Grammar
    , Rules
    , noRules
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
import Match exposing (Adapter, Token)

type alias Parser o =
    String -> ParseResult o

type alias ParserDef o = ( Grammar o, String, Maybe (Adapter o) )

init : ParserDef o
init =
    (noRules, "start", Nothing)

fromDefinition : ParserDef o -> Parser o
fromDefinition =
    parseWith

parseWith : ParserDef o -> String -> ParseResult o
parseWith ( grammar, startRule, maybeAdapter ) input =
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

-- FIXME: change ParseResult to some type which returns Matched | Failed (FailureReason, Position)
--        may be change ParseResult to `OpParseResult or OpSuccess = OpMatched | OpFailed` and keep --        it private.
--        Fix the docs in the intro then.
parse : String -> ParserDef o -> ParseResult o
parse input def =
    input |> fromDefinition def

withRules : Rules o -> ParserDef o -> ParserDef o
withRules rules ( _, startRule, maybeAdapter ) =
    ( Dict.fromList rules, startRule, maybeAdapter )
    -- { parser | grammar = Dict.fromList rules }

    -- , startRule = case List.head rules of
    --     Just ( name, _ ) -> name
    --     Nothing -> "start"

start : Operator o -> ParserDef o
start op =
    init |> startWith op

startWith : Operator o -> ParserDef o -> ParserDef o
startWith op def =
    def |> addRule "start" op

addStartRule : Operator o -> ParserDef o -> ParserDef o
addStartRule = startWith

setStartRule : RuleName -> ParserDef o -> ParserDef o
setStartRule name ( grammar, _, maybeAdapter ) =
    ( grammar, name, maybeAdapter )

addRule : RuleName -> Operator o -> ParserDef o -> ParserDef o
addRule name op ( grammar, startRule, maybeAdapter ) =
    ( grammar |> Dict.insert name op, startRule, maybeAdapter )

getRule : RuleName -> ParserDef o -> Maybe (Operator o)
getRule name ( grammar, _, _ ) =
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
