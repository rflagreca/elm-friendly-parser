module Execute exposing
    ( Context
    , execute
    , getCurrentChar
    , toResult
    , failByEndOfInput -- FIXME: should not be exported
    )

import Regex
import Dict exposing (Dict)

import Grammar exposing
    ( Grammar, getRule
    )
import State exposing
    ( State
    , Position
    , findPosition
    )
import Operator exposing
    ( Rule, RuleName
    , Operator(..)
    )
import Action exposing
    ( ActionResult(..)
    , PrefixActionResult(..)
    , UserCode
    , UserPrefixCode
    )
import ParseResult exposing
    ( Expectation(..)
    , FailureReason(..)
    , Sample(..)
    )
import Match exposing
    ( Adapter
    , Token(..)
    )

type StepResult o = Matched (Token o) | Failed (FailureReason o)

type alias Context o =
    { grammar: Grammar o
    , state: State o
    , adapter: Maybe (Adapter o)
    }

type alias OperatorResult o = ( StepResult o, State o )

-- OPERATORS EXECUTION

execute : Operator o -> Context o -> OperatorResult o
execute op ctx =
    ctx |> case op of
        NextChar -> execNextChar -- `ch`
        Match str -> execMatch str -- `match`
        Sequence ops -> execSequence ops -- `seqnc`
        Choice ops -> execChoice ops -- `choice`
        Maybe_ op -> execMaybe op -- `maybe`
        TextOf op -> execTextOf op -- `text`
        Some op -> execSome op -- `some`
        Any op -> execAny op -- `any`
        And op -> execAnd op -- `and`
        Not op -> execNot op -- `not`
        Action op uc -> execAction op uc -- `action`
        PreExec uc -> execPre uc -- `pre`
        NegPreExec uc -> execNegPre uc -- `xpre`
        Label n op -> execLabel n op -- `label`
        Call n -> execCall n -- `call` a.k.a. `ref`
        CallAs n1 n2 -> execCallAs n1 n2
        Regex re desc -> execRegex re desc

execNextChar : Context o -> OperatorResult o
execNextChar ctx =
    let
        { state } = ctx
    in
        if (state.position >= state.inputLength) then
            state |> failedBy ExpectedAnything GotEndOfInput
        else
            state |> advanceBy 1 |> matched (Lexem (getNextChar state))

execMatch : String -> Context o -> OperatorResult o
execMatch expectation ctx =
    let
        { state } = ctx
        inputLength = state.inputLength
        expectationLength = String.length expectation
    in
        if (state.position + expectationLength) > inputLength then
            state |> failedBy (ExpectedValue expectation) GotEndOfInput
        else
            if (String.startsWith expectation
                (state.input |> String.dropLeft state.position)) then
                state |> advanceBy expectationLength |> matched (Lexem expectation)
            else
                state |> failedCC (ExpectedValue expectation)

execSequence : List (Operator o) -> Context o -> OperatorResult o
execSequence ops ctx =
    let
        { state } = ctx
    in
        case ops of
            [] -> state |> failedBy ExpectedAnything GotEndOfInput
            (firstOp::restOps) ->
                let
                    applied = chain
                        (\prevResult lastState reducedVal ->
                            let
                                ( opsLeft, maybeFailed, matches, _ ) = reducedVal
                            in
                                case ( prevResult, opsLeft ) of
                                    ( Matched v, [] ) ->
                                        StopWith ( [], Nothing, matches ++ [ v ], lastState )
                                    ( Matched v, nextOp::restOps ) ->
                                        Next ( nextOp, ( restOps, Nothing, matches ++ [ v ], lastState ) )
                                    ( Failed failure, _ ) ->
                                        StopWith ( [], Just failure, matches, lastState )
                        )
                        firstOp ( restOps, Nothing, [], state ) ctx
                in
                    case applied of
                        ( _, Nothing, matches, lastState ) ->
                            lastState |> matched (Tokens matches)
                        ( _, Just reason, failures, lastState ) ->
                            state |> movePosition lastState |> failed reason

execChoice : List (Operator o) -> Context o -> OperatorResult o
execChoice ops ctx =
    case ops of
        [] -> ctx.state |> failedBy ExpectedAnything GotEndOfInput
        (firstOp::restOps) ->
            let
                applied = chain
                    (\prevResult lastCtx reducedVal ->
                        let
                            ( opsLeft, maybeMatched, failures ) = reducedVal
                        in
                            case ( prevResult, opsLeft ) of
                                ( Matched v, _ ) ->
                                    StopWith ( [], Just (v, lastCtx), failures )
                                ( Failed failure, [] ) ->
                                    case maybeMatched of
                                        Just v -> StopWith ( [], maybeMatched, failures )
                                        Nothing -> StopWith ( [], Nothing, failures ++ [ prevResult ] )
                                ( Failed failure, nextOp::restOps ) ->
                                    Next ( nextOp, ( restOps, Nothing, failures ++ [ prevResult ] ) )
                    )
                    firstOp ( restOps, Nothing, [] ) ctx
            in
                case applied of
                    ( _, Just ( success, lastCtx ), _ ) -> lastCtx |> matched success
                        -- ctx |> loadPosition lastCtx |> matchedWith success
                    ( _, Nothing, failures ) ->
                        ctx.state |> failedNestedCC (keepOnlyFailures failures)

execSome : Operator o -> Context o -> OperatorResult o
execSome op ctx =
    let
        applied = chain
                  (\prevResult lastCtx reducedVal ->
                      case prevResult of
                          Matched v ->
                            case reducedVal of
                                ( prevMatches, _, _ ) ->
                                    Next ( op, ( prevMatches ++ [ v ], Just lastCtx, Nothing ) )
                          Failed f ->
                            case reducedVal of
                                ( [], _, _ ) -> StopWith ( [], Nothing, Just f )
                                _ -> Stop
                  )
                  op ( [], Nothing, Nothing ) ctx
    in
        case applied of
            ( allMatches, Just lastCtx, Nothing ) -> lastCtx |> matched (Tokens allMatches)
            ( _, _, Just failure ) -> ctx.state |> failed failure
            _ -> ctx.state |> failedBy ExpectedAnything GotEndOfInput

execAny : Operator o -> Context o -> OperatorResult o
execAny op ctx =
    let
        someResult = (execSome op ctx)
    in
        case someResult of
            ( Matched _, _ ) -> someResult
            ( Failed _, _ ) -> ctx.state |> matched (Tokens [])

execMaybe : Operator o -> Context o -> OperatorResult o
execMaybe op ctx =
    let
        result = execute op ctx
    in
        case result of
            ( Matched s, stateAfterOp ) -> stateAfterOp |> matched s
            ( Failed _, _ ) -> ctx.state |> matched NoLexem

execTextOf : Operator o -> Context o -> OperatorResult o
execTextOf op ctx =
    let
        { state }  = ctx
        prevPos = state.position
        result = execute op ctx
    in
        case result of
            ( Matched s, stateAfterOp ) ->
                stateAfterOp |> matched (Lexem
                        (stateAfterOp.input |> String.slice prevPos stateAfterOp.position))
            failure -> failure

execAnd : Operator o -> Context o -> OperatorResult o
execAnd op ctx =
    let
        ( result, stateAfterOp ) = (execute op ctx)
    in
        case result of
            Matched v -> stateAfterOp |> matched NoLexem
            failure -> ( failure, stateAfterOp )

execNot : Operator o -> Context o -> OperatorResult o
execNot op ctx =
    let
        ( result, _ ) = (execute op ctx)
    in
        case result of
            Matched _ -> ctx.state |> failedCC ExpectedEndOfInput
            failure -> ctx.state |> matched NoLexem

execAction : Operator o -> UserCode o -> Context o -> OperatorResult o
execAction op userCode ctx =
    let
        ( result, stateAfterOp ) = (execute op ctx)
         -- we forget all the data left inside the "closure" and take only the new position
        resultingState = ctx.state |> movePosition stateAfterOp
    in
        case result of
            Matched v ->
                case (userCode v stateAfterOp) of
                    Pass userV -> resultingState |> matched (My userV)
                    PassThrough -> resultingState |> matched v
                    Fail -> resultingState |> failedCC ExpectedAnything
            Failed _ -> ( result, resultingState )

execPre : UserPrefixCode o -> Context o -> OperatorResult o
execPre userCode ctx =
    let
        { state } = ctx
        result = (userCode state)
    in
        case result of
            Continue -> state |> matched NoLexem
            Halt -> state |> failedCC ExpectedEndOfInput

execNegPre : UserPrefixCode o -> Context o -> OperatorResult o
execNegPre userCode ctx =
    let
        { state } = ctx
        result = (userCode state)
    in
        case result of
            Continue -> state |> failedCC ExpectedEndOfInput
            Halt -> state |> matched NoLexem

execLabel : String -> Operator o -> Context o -> OperatorResult o
execLabel name op ctx =
    let
        ( result, stateAfterOp ) = (execute op ctx)
        stateMaybeWithValue =
            case result of
                Matched v ->
                    { stateAfterOp
                    | values = stateAfterOp.values |> Dict.insert name v
                    }
                Failed _ -> stateAfterOp
    in
        ( result, stateMaybeWithValue )

execCall : RuleName -> Context o -> OperatorResult o
execCall ruleName ctx =
    execCallAs ruleName ruleName ctx

execCallAs : RuleName -> RuleName -> Context o -> OperatorResult o
execCallAs ruleAlias realRuleName ctx =
    let
        { grammar, state } = ctx
    in
        case (getRule realRuleName grammar) of
            Just op -> (execute op ctx) |> addRuleToResult ruleAlias
            Nothing -> state |> failedBy (ExpectedRuleDefinition realRuleName) (gotChar state)

-- execDefineRule : RuleName -> Operator o -> Context o -> OperatorResult o
-- execDefineRule ruleName op ctx =
--     matched "" { ctx | rules = ctx.rules |> addRule_ ruleName op }

execRegex : String -> Maybe String -> Context o -> OperatorResult o
execRegex regex maybeDesc ctx =
    -- FIXME: cache all regular expressions with Regex.Regex instances
    let
        { state } = ctx
        regexInstance = Regex.regex regex
        matches = Regex.find (Regex.AtMost 1) regexInstance
                    (String.slice state.position state.inputLength state.input)
        -- FIXME: add `^` to the start, so Regex with try matching from the start,
        --        which should be faster
        firstMatch = List.head matches
        description = case maybeDesc of
                        Just d -> d
                        Nothing -> regex
    in
        case firstMatch of
            Just match ->
                if match.index == 0 then
                    state
                        |> advanceBy (String.length match.match)
                        |> matched (Lexem match.match)
                else
                    state |> failedRE description
            Nothing -> state |> failedRE description


-- MATCHING UTILS

matched : Token o -> State o -> OperatorResult o
matched token state =
    ( Matched token, state )

failed : FailureReason o -> State o -> OperatorResult o
failed reason state =
    ( Failed reason, state )

failedBy : Expectation -> Sample -> State o -> OperatorResult o
failedBy expectation sample state =
    state |> failed (ByExpectation ( expectation, sample ))

-- fail with current character
failedCC : Expectation -> State o -> OperatorResult o
failedCC expectation state =
    state |> failedBy expectation (gotChar state)

failedNested : List (FailureReason o) -> Sample -> State o -> OperatorResult o
failedNested failures sample state =
    state |> failed (FollowingNestedOperator ( failures, sample ))

failedNestedCC : List (FailureReason o) -> State o -> OperatorResult o
failedNestedCC failures state =
    state |> failedNested failures (gotChar state)

failedRE : String -> State o -> OperatorResult o
failedRE desc state =
    state |> failedCC (ExpectedRegexMatch desc)

notImplemented : State o -> OperatorResult o
notImplemented state =
    state |> failed SomethingWasNotImplemented

-- failWith : Expectation -> Sample -> ParseResult
-- failWith expectation sample =
--     ExpectationFailure ( expectation, sample )

advanceBy : Int -> State o -> State o
advanceBy count state =
    { state | position = state.position + count }

getNextSubstring : State o -> Int -> Int -> String
getNextSubstring state shift count =
    String.slice (state.position + shift) (state.position + shift + count) state.input

getNextChar : State o -> String
getNextChar ctx =
    getNextSubstring ctx 1 1

getCurrentChar : State o -> String
getCurrentChar ctx =
    getNextSubstring ctx 0 1

gotChar : State o -> Sample
gotChar state =
    GotValue (getCurrentChar state)

addRuleToResult : RuleName -> OperatorResult o -> OperatorResult o
addRuleToResult ruleName ( result, state ) =
    case result of
        Matched v -> state |> matched (InRule ruleName v)
        Failed failure -> ( Failed (FollowingRule ruleName failure), state )

failByEndOfInput : State o -> ( FailureReason o, Position )
failByEndOfInput state =
    ( ByExpectation
        (ExpectedEndOfInput, (GotValue (getCurrentChar state)))
    , findPosition state
    )

-- HELPERS

toResult : StepResult o -> Result (FailureReason o) (Token o)
toResult stepResult =
    case stepResult of
        Matched v -> Ok v
        Failed f -> Err f

movePosition : State o -> State o -> State o
movePosition { position } state =
    { state | position = position }

-- TODO: type Step o v f = TryNext ( Operator o, v ) | Success v | Success | Failure f
--       chain should return Success or Failure then
type Step o v = Next ( Operator o, v ) | StopWith v | Stop

chain :
    -- (StepResult o -> Context o -> v -> ChainStep (Operator o, v))
       (StepResult o -> State o -> v -> Step o v)
    -> Operator o
    -> v
    -> Context o
    -> v
chain stepFn initialOp initialVal ctx =
    let
        initialState = ctx.state
        unfold = (\op state val ->
                    let
                        ( mayBeMatched, nextState ) = ( execute op ctx )
                    in
                        case (stepFn mayBeMatched nextState val) of
                            Next (nextOp, nextVal) ->
                                (unfold nextOp nextState nextVal)
                            StopWith lastVal -> lastVal
                            Stop -> val

                    )
    in
        unfold initialOp initialState initialVal

-- concat : StepResult o -> StepResult o -> Context o -> OperatorResult o
-- concat resultOne resultTwo inContext =
--     case ( resultOne, resultTwo ) of
--         ( Matched vOne, Matched vTwo ) ->
--             matchedList [ vOne, vTwo ] inContext
--         _ -> ( resultTwo, inContext )

keepOnlyMatches : List (StepResult o) -> List (Token o)
keepOnlyMatches parseResults =
    List.filterMap
        (\result ->
            case result of
                Matched v -> Just v
                Failed _ -> Nothing)
        parseResults

keepOnlyFailures : List (StepResult o) -> List (FailureReason o)
keepOnlyFailures parseResults =
    List.filterMap
        (\result ->
            case result of
                Matched _ -> Nothing
                Failed failure -> Just failure)
        parseResults
