module Operator exposing
    ( Operator(..)
    , Context
    , RuleName, Rule, Grammar, Rules, noRules
    , ch, match, choice, seqnc, maybe, text, any, some, and, not
    , action, pre, xpre, label, call, re, redesc
    , execute
    , getCurrentChar
    , toResult
    , failByEndOfInput
    )

 {- TODO: rename module to Grammar -}

import Dict exposing (Dict)
import Regex

import State exposing
    ( State
    , Position
    , findPosition
    )
import Match exposing (Adapter, Token(..))
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

type alias RuleName = String
type alias Rule o = ( RuleName, Operator o )
type alias Grammar o = Dict RuleName (Operator o)
type alias Rules o = List (Rule o)

type StepResult o = Matched (Token o) | Failed (FailureReason o)

type alias Context o =
    { grammar: Grammar o
    , state: State o
    , adapter: Maybe (Adapter o)
    }

type alias OperatorResult o = ( StepResult o, State o )

type Operator o =
      NextChar -- 1. `ch`
    | Match String -- 2. `match`
    | Regex String (Maybe String) -- 3. `re`, `redesc`
    | TextOf (Operator o) -- 4. `text`
    | Maybe_ (Operator o) -- 5. `maybe`
    | Some (Operator o) -- 6. `some`
    | Any (Operator o)  -- 7. `any`
    | And (Operator o) -- 8. `and`
    | Not (Operator o) -- 9. `not`
    | Sequence (List (Operator o)) -- 10. `seqnc`
    | Choice (List (Operator o)) -- 11. `choice`
    | Action (Operator o) (UserCode o) -- 12. `action`
    | PreExec (UserPrefixCode o) -- 13. `pre`
    | NegPreExec (UserPrefixCode o) -- 14. `xpre`
    | Label String (Operator o) -- 15. `label`
    -- | Rule RuleName (Operator o) -- 16. `rule`
    | Call RuleName -- 17. `call` a.k.a `ref`
    -- | Alias String (Operator o) -- 18. `as`
    | CallAs RuleName RuleName

match : String -> Operator o
match subject =
    Match subject

ch : Operator o
ch =
    NextChar

re : String -> Operator o
re regex_ =
    Regex regex_ Nothing

redesc : String -> String -> Operator o
redesc regex_ description =
    Regex regex_ (Just description)

seqnc : List (Operator o) -> Operator o
seqnc operators =
    Sequence operators

choice : List (Operator o) -> Operator o
choice operators =
    Choice operators


maybe : Operator o -> Operator o
maybe operator =
    Maybe_ operator

any : Operator o -> Operator o
any operator =
    Any operator

some : Operator o -> Operator o
some operator =
    Some operator

and : Operator o -> Operator o
and operator =
    And operator

not : Operator o -> Operator o
not operator =
    Not operator

-- FIXME: make `call` accept the rule from the RulesList
call : RuleName -> Operator o
call ruleName =
    Call ruleName

-- FIXME: actions should have access to a position, check the examples.
action : Operator o -> UserCode o -> Operator o
action operator userCode =
    Action operator userCode

pre : UserPrefixCode o -> Operator o
pre userCode =
    PreExec userCode

xpre : UserPrefixCode o -> Operator o
xpre userCode =
    NegPreExec userCode

text : Operator o -> Operator o
text operator =
    TextOf operator

-- FIXME: check the examples
label : String -> Operator o -> Operator o
label name operator =
    Label name operator

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

-- UTILS

-- FIMXE: the functions which need no whole context, should use just State
---       (and may be moved to State module)

noRules : Grammar o
noRules = Dict.empty

matched : Token o -> State o -> OperatorResult o
matched token state =
    ( Matched token, state )

-- matchedFlatList : List o -> Context o -> OperatorResult o
-- matchedFlatList val ctx =
--     matchedWith (ctx.flatten (AList val)) ctx

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

getRule : RuleName -> Grammar o -> Maybe (Operator o)
getRule name grammar =
    Dict.get name grammar

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
