module Operator exposing (..)

import Dict exposing (Dict)
import Regex

import User exposing (..)

type alias Rules o = Dict RuleName (Operator o)
type alias RulesList o = List ( RuleName, Operator o )

-- FIXME: Merge Parser and Context in a Parser or Context record ??

type alias Context o =
    { input: String
    , inputLength: Int
    , position: Int
--    , previousPosition: Int
    , rules: Rules o
    , values: Values o
    , adapt: Adapter o
}

type ActionResult o = Pass o | PassThrough | Fail -- Return o | PassThrough | Fail
type PrefixActionResult = Continue | Halt -- Continue | Stop (change ChainStep name to End or Exit/ExitWith)

type alias OperatorResult o = (ParseResult o, Context o)

type alias UserCode o = (o -> Context o -> (ActionResult o))
type alias UserPrefixCode o = (Context o -> PrefixActionResult)

type OperatorType o =
      NextChar -- 1. `ch`
    | Match String -- 2. `match`
    | Regex String (Maybe String) -- 3. `re`
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

type alias Operator o = OperatorType o

type Expectation =
      ExpectedValue String -- FIXME: InputType?
    | ExpectedAnything
    | ExpectedRuleDefinition RuleName
    | ExpectedRegexMatch String
    --| ExpectedStartRule
    | ExpectedEndOfInput

type Sample =
      GotValue String
    | GotEndOfInput

type FailureReason o =
      ByExpectation ( Expectation, Sample )
    | FollowingRule RuleName (FailureReason o)
    | FollowingNestedOperator ( List (ParseResult o), Sample )
    | NoStartRule
    | SomethingWasNotImplemented

type ParseResult o =
      Matched o
    | Failed (FailureReason o)

-- OPERATORS

ch : Operator o
ch =
    NextChar

match : String -> Operator o
match subject =
    Match subject

choice : List (Operator o) -> Operator o
choice operators =
    Choice operators

seqnc : List (Operator o) -> Operator o
seqnc operators =
    Sequence operators

maybe : Operator o -> Operator o
maybe operator =
    Maybe_ operator

text : Operator o -> Operator o
text operator =
    TextOf operator

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

action : Operator o -> UserCode o -> Operator o
action operator userCode =
    Action operator userCode

pre : UserPrefixCode o -> Operator o
pre userCode =
    PreExec userCode

xpre : UserPrefixCode o -> Operator o
xpre userCode =
    NegPreExec userCode

label : String -> Operator o -> Operator o
label name operator =
    Label name operator

call : RuleName -> Operator o
call ruleName =
    Call ruleName

re : String -> Operator o
re regex_ =
    Regex regex_ Nothing

redesc : String -> String -> Operator o
redesc regex_ description =
    Regex regex_ (Just description)

-- rule : RuleName -> Operator o -> Operator o
-- rule ruleName op =
--     Rule ruleName op

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
    if (ctx.position >= ctx.inputLength) then
        ctx |> failedBy ExpectedAnything GotEndOfInput
    else
        ctx |> advanceBy 1 |> matched (getNextChar ctx)

execMatch : String -> Context o -> OperatorResult o
execMatch expectation ctx =
    let
        inputLength = ctx.inputLength
        expectationLength = String.length expectation
    in
        if (ctx.position + expectationLength) > inputLength then
            ctx |> failedBy (ExpectedValue expectation) GotEndOfInput
        else
            if (String.startsWith expectation
                (ctx.input |> String.dropLeft ctx.position)) then
                ctx |> advanceBy expectationLength |> matched expectation
            else
                ctx |> failedCC (ExpectedValue expectation)

execSequence : List (Operator o) -> Context o -> OperatorResult o
execSequence ops ctx =
    case ops of
        [] -> ctx |> failedBy ExpectedAnything GotEndOfInput
        (firstOp::restOps) ->
            let
                applied = chain
                    (\prevResult lastCtx reducedVal ->
                        let
                            ( opsLeft, maybeFailed, matches, _ ) = reducedVal
                        in
                            case ( prevResult, opsLeft ) of
                                ( Matched v, [] ) ->
                                    StopWith ( [], Nothing, matches ++ [ v ], lastCtx )
                                ( Matched v, nextOp::restOps ) ->
                                    Next ( nextOp, ( restOps, Nothing, matches ++ [ v ], lastCtx ) )
                                ( Failed failure, _ ) ->
                                    StopWith ( [], Just failure, matches, lastCtx )
                    )
                    firstOp ( restOps, Nothing, [], ctx ) ctx
            in
                case applied of
                    ( _, Nothing, matches, lastCtx ) -> lastCtx |> matchedList matches
                    ( _, Just reason, failures, _ ) -> ctx |> failed reason

execChoice : List (Operator o) -> Context o -> OperatorResult o
execChoice ops ctx =
    case ops of
        [] -> ctx |> failedBy ExpectedAnything GotEndOfInput
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
                    ( _, Just ( success, lastCtx ), _ ) -> lastCtx |> matchedWith success
                    ( _, Nothing, failures ) -> ctx |> failedNestedCC failures

execSome : Operator o -> Context o -> OperatorResult o
execSome op ctx =
    let
        applied = chain
                  (\prevResult lastCtx reducedVal ->
                      case prevResult of
                          Matched v ->
                            case reducedVal of
                                ( prevMatches, _, _ ) ->
                                    Next ( op, ( [ v ] ++ prevMatches, Just lastCtx, Nothing ) )
                          Failed f ->
                            case reducedVal of
                                ( [], _, _ ) -> StopWith ( [], Nothing, Just f )
                                _ -> Stop
                  )
                  op ( [], Nothing, Nothing ) ctx
    in
        case applied of
            ( allMatches, Just lastCtx, Nothing ) -> lastCtx |> matchedList allMatches
            ( _, _, Just failure ) -> ctx |> failed failure
            _ -> ctx |> failedBy ExpectedAnything GotEndOfInput

execAny : Operator o -> Context o -> OperatorResult o
execAny op ctx =
    let
        someResult = (execSome op ctx)
    in
        case someResult of
            ( Matched _, _ ) -> someResult
            ( Failed _, _ ) -> ctx |> matched ""

execMaybe : Operator o -> Context o -> OperatorResult o
execMaybe op ctx =
    let
        result = execute op ctx
    in
        case result of
            ( Matched s, newCtx ) -> matchedWith s newCtx
            ( Failed _, _ ) -> matched "" ctx

execTextOf : Operator o -> Context o -> OperatorResult o
execTextOf op ctx =
    let
        prevPos = ctx.position
        result = execute op ctx
    in
        case result of
            ( Matched s, newCtx ) ->
                newCtx |> matched
                    (newCtx.input |> String.slice prevPos newCtx.position)
            failure -> failure

execAnd : Operator o -> Context o -> OperatorResult o
execAnd op ctx =
    let
        ( result, newCtx ) = (execute op ctx)
    in
        case result of
            Matched v -> matched "" ctx
            failure -> ( failure, newCtx )

execNot : Operator o -> Context o -> OperatorResult o
execNot op ctx =
    let
        ( result, newCtx ) = (execute op ctx)
    in
        case result of
            Matched _ -> newCtx |> failedCC ExpectedEndOfInput
            failure -> matched "" ctx

execAction : Operator o -> UserCode o -> Context o -> OperatorResult o
execAction op userCode ctx =
    let
        ( result, newCtx ) = (execute op ctx)
    in
        case result of
            Matched v ->
                case (userCode v newCtx) of
                    Pass userV -> newCtx |> matchedWith userV
                    PassThrough -> newCtx |> matchedWith v
                    Fail -> newCtx |> failedCC ExpectedAnything
            Failed _ -> ( result, newCtx )

execPre : UserPrefixCode o -> Context o -> OperatorResult o
execPre userCode ctx =
    let
        result = (userCode ctx)
    in
        case result of
            Continue -> ctx |> matched ""
            Halt -> ctx |> failedCC ExpectedEndOfInput

execNegPre : UserPrefixCode o -> Context o -> OperatorResult o
execNegPre userCode ctx =
    let
        result = (userCode ctx)
    in
        case result of
            Continue -> ctx |> failedCC ExpectedEndOfInput
            Halt -> ctx |> matched ""

execLabel : String -> Operator o -> Context o -> OperatorResult o
execLabel name op ctx =
    let
        ( result, newCtx ) = (execute op ctx)
        updatedCtx =
            case result of
                Matched v ->
                    { newCtx | values = newCtx.values |> Dict.insert name v }
                Failed _ -> newCtx
    in
        ( result, updatedCtx )

execCall : RuleName -> Context o -> OperatorResult o
execCall ruleName ctx =
    execCallAs ruleName ruleName ctx

execCallAs : RuleName -> RuleName -> Context o -> OperatorResult o
execCallAs ruleAlias realRuleName ctx =
    case (getRule realRuleName ctx) of
        Just op -> (execute op ctx) |> addRuleToResult ruleAlias
        Nothing -> ctx |> failedBy (ExpectedRuleDefinition realRuleName) (gotChar ctx)

-- execDefineRule : RuleName -> Operator o -> Context o -> OperatorResult o
-- execDefineRule ruleName op ctx =
--     matched "" { ctx | rules = ctx.rules |> addRule_ ruleName op }

execRegex : String -> Maybe String -> Context o -> OperatorResult o
execRegex regex maybeDesc ctx =
    -- FIXME: cache all regular expressions with Regex.Regex instances
    let
        regexInstance = Regex.regex regex
        matches = Regex.find (Regex.AtMost 1) regexInstance
                    (String.slice ctx.position ctx.inputLength ctx.input)
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
                    ctx |> matched match.match
                else
                    ctx |> failedRE description
            Nothing -> ctx |> failedRE description

-- UTILS

-- TODO: type Step o v f = TryNext ( Operator o, v ) | Success v | Success | Failure f
--       chain should return Success or Failure then
type Step o v = Next ( Operator o, v ) | StopWith v | Stop

chain :
    -- (ParseResult o -> Context o -> v -> ChainStep (Operator o, v))
       (ParseResult o -> Context o -> v -> Step o v)
    -> Operator o
    -> v
    -> Context o
    -> v
chain stepFn initialOp initialVal initialCtx =
    let
        unfold = (\op ctx val ->
                    let
                        ( mayBeMatched, nextCtx ) = ( execute op ctx )
                    in
                        case (stepFn mayBeMatched nextCtx val) of
                            Next (nextOp, nextVal) ->
                                (unfold nextOp nextCtx nextVal)
                            StopWith lastVal -> lastVal
                            Stop -> val

                    )
    in
        unfold initialOp initialCtx initialVal

isNotParsed : ParseResult o -> Bool
isNotParsed result =
    case result of
        Matched _ -> False
        Failed _ -> True

isParsedAs : String -> ParseResult o -> Bool
isParsedAs subject result =
    case result of
        Matched s -> (toString s == subject)
        Failed _-> False

advanceBy : Int -> Context o -> Context o
advanceBy cnt ctx =
    { ctx | position = ctx.position + cnt }

getNextChar : Context o -> String
getNextChar ctx =
    String.slice (ctx.position + 1) (ctx.position + 2) ctx.input

getCurrentChar : Context o -> String
getCurrentChar ctx =
    String.slice ctx.position (ctx.position + 1) ctx.input

gotChar : Context o -> Sample
gotChar ctx =
    GotValue (getCurrentChar ctx)

extractParseResult : OperatorResult o -> ParseResult o
extractParseResult opResult =
    Tuple.first opResult

extractContext : OperatorResult o -> Context o
extractContext opResult =
    Tuple.second opResult

matchedWith : o -> Context o -> OperatorResult o
matchedWith output ctx =
    ( Matched output, ctx )

matched : String -> Context o -> OperatorResult o
matched val ctx =
    matchedWith (ctx.adapt (AValue val)) ctx

matchedList : List o -> Context o -> OperatorResult o
matchedList val ctx =
    matchedWith (ctx.adapt (AList val)) ctx

matchedRule : RuleName -> o -> Context o -> OperatorResult o
matchedRule ruleName value ctx =
    matchedWith (ctx.adapt (ARule ruleName value)) ctx

-- matchedFlatList : List o -> Context o -> OperatorResult o
-- matchedFlatList val ctx =
--     matchedWith (ctx.flatten (AList val)) ctx

failed : FailureReason o -> Context o -> OperatorResult o
failed reason ctx =
    ( Failed reason, ctx )

failedBy : Expectation -> Sample -> Context o -> OperatorResult o
failedBy expectation sample ctx =
    ctx |> failed (ByExpectation ( expectation, sample ))

-- fail with current character
failedCC : Expectation -> Context o -> OperatorResult o
failedCC expectation ctx =
    ctx |> failedBy expectation (gotChar ctx)

failedNested : List (ParseResult o) -> Sample -> Context o -> OperatorResult o
failedNested failures sample ctx =
    ctx |> failed (FollowingNestedOperator ( failures, sample ))

failedNestedCC : List (ParseResult o) -> Context o -> OperatorResult o
failedNestedCC failures ctx =
    ctx |> failedNested failures (gotChar ctx)

failedRE : String -> Context o -> OperatorResult o
failedRE desc ctx =
    ctx |> failedCC (ExpectedRegexMatch desc)

notImplemented : Context o -> OperatorResult o
notImplemented ctx =
    ctx |> failed SomethingWasNotImplemented

-- failWith : Expectation -> Sample -> ParseResult
-- failWith expectation sample =
--     ExpectationFailure ( expectation, sample )

addRuleToResult : RuleName -> OperatorResult o -> OperatorResult o
addRuleToResult ruleName ( result, ctx ) =
    case result of
        Matched v -> ctx |> matchedRule ruleName v
        Failed failure -> ( Failed (FollowingRule ruleName failure), ctx )

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

concat : ParseResult o -> ParseResult o -> Context o -> OperatorResult o
concat resultOne resultTwo inContext =
    case ( resultOne, resultTwo ) of
        ( Matched vOne, Matched vTwo ) ->
            matchedList [ vOne, vTwo ] inContext
        _ -> ( resultTwo, inContext )

addRule : RuleName -> Operator o -> Rules o -> Rules o
addRule name op rules =
    rules |> Dict.insert name op

getRule : RuleName -> Context o -> Maybe (Operator o)
getRule name ctx =
    Dict.get name ctx.rules

