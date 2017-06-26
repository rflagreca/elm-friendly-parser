module Core.Parser exposing
    ( Parser, init, start, startWith, parse
    -- , Position, ParseResult(..), FailureReason(..), Expectation(..), Sample(..)
    , withRules, setStartRule, getStartRule, getRule, noRules --, RuleName, Rules, RulesList
    -- , ActionResult(..), PrefixActionResult(..), UserCode, UserPrefixCode
    -- , InputType(..)
    -- , Adapter
    -- , Operator(..), State
    )

import Dict exposing (..)
import Regex

import Core.Adapter exposing (Adapter)
import Core.Operator exposing (Operator, Rules, execute)
import Core.State as State exposing (State)
import Core.Result exposing (Position, FailureReason(..), Expectation(..), Sample(..))

type alias Parser o =
    { adapt: Adapter o
    , rules: Rules o
    , startRule: String
    }

init : Adapter o -> Parser o
init adapter =
    { adapt = adapter
    , rules = noRules
    , startRule = "start"
    }

type alias Context o = ( Parser o, State o )

type ParseResult o =
      Matched o
    | Failed (FailureReason o) Position

-- FIXME: change ParseResult to some type which returns Matched | Failed (FailureReason, Position)
--        may be change ParseResult to `OpParseResult or OpSuccess = OpMatched | OpFailed` and keep --        it private.
--        Fix the docs in the intro then.
parse : String -> Parser o -> ParseResult o
parse input parser =
    let
        state = (State.init input)
        context = (parser, state)
    in
        case getStartRule parser of
            Just startOperator ->
                -- TODO: extractParseResult (execCall parser.startRule context)
                let
                    ( parseResult, lastCtx ) = (execute startOperator context)
                    ( _, lastState ) = lastCtx
                in
                    case parseResult of
                        Matched success ->
                            if lastState.position == (String.length input) then
                                ( parseResult, Nothing )
                            else
                                ( Failed (ByExpectation
                                    (ExpectedEndOfInput, (GotValue (getCurrentChar lastCtx))))
                                , Just (findPosition lastState)
                                )
                        Failed _ -> ( parseResult, Just (findPosition lastState) )
            Nothing -> ( Failed NoStartRule, Nothing )

noRules : Rules o
noRules = Dict.empty

withRules : RulesList o -> Parser o -> Parser o
withRules rules parser =
    { parser | rules = Dict.fromList rules }
    -- , startRule = case List.head rules of
    --     Just ( name, _ ) -> name
    --     Nothing -> "start"

start : Operator o -> Adapter o -> Parser o
start op adapter =
    init adapter |> startWith op

startWith : Operator o -> Parser o -> Parser o
startWith op parser =
    parser |> addRule "start" op

addStartRule : Operator o -> Parser o -> Parser o
addStartRule = startWith

getStartRule : Parser o -> Maybe (Operator o)
getStartRule parser =
    Dict.get parser.startRule parser.rules

setStartRule : RuleName -> Parser o -> Parser o
setStartRule name parser =
    { parser | startRule = name }

addRule : RuleName -> Operator o -> Parser o -> Parser o
addRule name op parser =
    { parser | rules = parser.rules |> Dict.insert name op }

getRule : RuleName -> Parser o -> Maybe (Operator o)
getRule name parser =
    Dict.get name parser.rules

-- UTILS

extractParseResult : OperatorResult o -> ParseResult o
extractParseResult opResult =
    Tuple.first opResult

extractContext : OperatorResult o -> Context o
extractContext opResult =
    Tuple.second opResult

opResultToMaybe : OperatorResult o -> ( Maybe o, Context o )
opResultToMaybe ( parseResult, ctx ) =
    ( parseResultToMaybe parseResult, ctx )

parseResultToMaybe : ParseResult o -> Maybe o
parseResultToMaybe result =
    case result of
        Matched v -> Just v
        Failed _ -> Nothing

parseResultToResult : ParseResult o -> Result (FailureReason o) o
parseResultToResult result =
    case result of
        Matched v -> Ok v
        Failed f -> Err f

findPosition : State o -> Position
findPosition state =
    let
        input = state.input
        allLines = String.lines input
        linesCount = List.length allLines
        curPosition = (state.position - (linesCount - 1)) -- '\n' count as separate symbols
    in
        .cursor
            (List.foldl
                (\line { cursor, prevCursor, sum } ->
                    if (sum >= curPosition) then
                        { cursor = prevCursor
                        , prevCursor = prevCursor
                        , sum = sum
                        }
                    else
                        case cursor of
                            ( lineIndex, charIndex ) ->
                                let
                                    strlen = (String.length line)
                                in
                                    if (sum + strlen) > curPosition then
                                        { cursor = ( lineIndex, curPosition - sum )
                                        , prevCursor = cursor
                                        , sum = sum + strlen
                                        }
                                    else
                                        { cursor = ( lineIndex + 1, 0 )
                                        , prevCursor = cursor
                                        , sum = sum + strlen
                                        }
                )
                { cursor = (0, 0)
                , prevCursor = (0, 0)
                , sum = 0
                }
                (String.lines input))
