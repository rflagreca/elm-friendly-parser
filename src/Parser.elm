module Parser exposing
    ( Parser, parse
    , withRules, withRulesAt, withAdapter
    , startFrom, startWithAdapter
    -- , Position, ParseResult(..), FailureReason(..), Expectation(..), Sample(..)
    , getRule --, noRules, RuleName, Rules, RulesList
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

withRules : Rules o -> Parser o
withRules rules =
    fromFriendlyDefinition (rules, Nothing, Nothing)

withRulesAt : Rules o -> String -> Parser o
withRulesAt rules startRule =
   fromFriendlyDefinition (rules, Just startRule, Nothing)

withAdapter : Rules o -> Adapter o -> Parser o
withAdapter rules adapter =
   fromFriendlyDefinition (rules, Nothing, Just adapter)

withAdapterAt : Rules o -> String -> Adapter o -> Parser o
withAdapterAt rules startRule adapter =
   fromFriendlyDefinition (rules, Just startRule, Just adapter)

startFrom : Operator o -> Parser o
startFrom startOp =
    fromDefinition
        ( noRules |> addRule "start" startOp
        , "start"
        , Nothing
        )

startWithAdapter : Operator o -> Adapter o -> Parser o
startWithAdapter startOp adapter =
    fromDefinition
        ( noRules |> addRule "start" startOp
        , "start"
        , Just adapter
        )

suggestStartRule : Grammar o -> Maybe String -> String
suggestStartRule grammar maybeName =
    case maybeName of
        Just name -> name
        Nothing ->
            case grammar |> getRule "start" of
                Just _ -> "start"
                Nothing -> List.head (Dict.keys grammar)
                    |> Maybe.withDefault "<UNKNOWN>"


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
