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

-- type alias ConfiguredParser o =
--     ParserDef o -> Parser o

-- type alias ParserDef o = ( Grammar o, String, Maybe (Adapter o) )

init : Rules o -> Parser o
init rules =
    fromFriendlyDefinition (rules, Nothing, Nothing)

init2 : Rules o -> String -> Parser o
init2 rules startRule =
   fromFriendlyDefinition (rules, Just startRule, Nothing)

init3 : Rules o -> Adapter o -> Parser o
init3 rules adapter =
   fromFriendlyDefinition (rules, Nothing, Just adapter)

init4 : Rules o -> String -> Adapter o -> Parser o
init4 rules startRule adapter =
   fromFriendlyDefinition (rules, Just startRule, Just adapter)

init5 : Operator o -> Parser o
init5 startOp =
    fromFriendlyDefinition
        ( noRules |> addRule "start" startOp
        , Just "start"
        , Nothing
        )

init6 : Operator o -> Adapter o -> Parser o
init6 startOp adapter =
    fromFriendlyDefinition
        ( noRules |> addRule "start" startOp
        , Just "start"
        , Just adapter
        )

suggestStartRule : Grammar o -> Maybe String -> String
suggestStartRule grammar maybeName =
    case maybeName of
        Just name -> name
        Nothing ->
            case grammar |> getRule "start" of
                Just _ -> "start"
                Nothing -> List.head Dict.keys |> Maybe.withDefault "<UNKNOWN>"


fromFriendlyDefinition : ( Rules o, Maybe String, Maybe (Adapter o) ) -> Parser o
fromFriendlyDefinition ( rules, maybeStartRule, maybeAdapter ) =
    let
        grammar = Dict.fromList rules
        startRule = maybeStartRule |> suggestStartRule grammar
    in
        fromDefinition ( grammar, startRule, maybeAdapter )

fromDefinition : ( Grammar o, String, Maybe (Adapter o) ) -> Parser o
fromDefinition =
    parseWith

parseWith : ( Grammar o, String, Maybe (Adapter o) ) -> String -> ParseResult o
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
parse : String -> Parser o -> ParseResult o
parse input parser =
    parser input

start : Operator o -> Parser o
start op =
    init |> startWith op

startWith : Operator o -> Parser o
startWith op =
    addRule "start" op

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
