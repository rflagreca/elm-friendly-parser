module Parser exposing (..)

import Dict exposing (..)
import Utils exposing (..)

type alias UserCode = (ParseResult -> Operator)

type OperatorType =
      NextChar -- 1. `ch` -- DONE
    | Match String -- 2. `match` -- DONE
    | Regex String String -- 3. `re`
    | TextOf Operator -- 4. `text` -- DONE
    | Maybe_ Operator -- 5. `maybe` -- DONE
    | Some Operator -- 6. `some`
    | Any Operator  -- 7. `any` -- DONE
    | And Operator -- 8. `and`
    | Not Operator -- 9. `not`
    | Sequence (List Operator) -- 10. `seqnc` -- DONE
    | Choice (List Operator) -- 11. `choice` -- DONE
    | Action Operator UserCode -- 12. `action`
    | PreExec UserCode -- 13. `pre`
    | NegPreExec UserCode -- 14. `xpre`
    | Label String Operator -- 15. `label`
    | Rule String Operator -- 16. `rule`
    | RuleReference String -- 17. `ref`
    | Alias String Operator -- 18. `as`

type alias Operator = OperatorType

type alias RuleName = String

type InputType o = AValue String | AList (List o)

-- type alias Chunk = ( Int, String )

type alias Adapter o = (InputType o -> o)

type alias Parser o = {
    adapt: Adapter o,
    rules: Rules
}

type Expectation =
      ExpectedValue String -- FIXME: InputType?
    -- | ExpectedList (List String)
    | ExpectedAnything
    -- | ExpectedRule RuleName
    -- | ExpectedStartRule
    -- | ExpectedOperator Operator
    | ExpectedEndOfInput
    -- | ExpectedChunk Chunk
    -- | ExpectedChunks (List Chunk)

type Sample =
      GotValue String
    | GotEndOfInput

type FailureReason o =
      ByExpectation ( Expectation, Sample )
    | FollowingNestedRule ( List (ParseResult o), Sample )
    | NoStartRule
    | SomethingWasNotImplemented

type ParseResult o =
      Matched o
    | Failed (FailureReason o)

-- FIXME: ParseResult should be Mathed | Failed pair, like Maybe or Result

-- type alias Context a = Dict String a
type alias Context o =
    { input: String
    , inputLength: Int
    , position: Int
    , rules: Rules
    , values: Values o
    , adapt: Adapter o
}

type alias OperatorResult o = (ParseResult o, Context o)

type alias Rules = Dict String Operator
type alias Values o = Dict String o

parse : Parser o -> String -> ParseResult o
parse parser input =
    let
        ctx = (initContext parser.adapt input)
    in
        case getStartRule parser of
            Just startOperator ->
                extractParseResult (execute startOperator ctx)
            Nothing -> Failed NoStartRule

-- RULES

noRules : Rules
noRules = Dict.empty

withRules : Rules -> Adapter o -> Parser o
withRules rules adapter =
    { adapt = adapter
    , rules = rules
    }

addRule : String -> Operator -> Rules -> Rules
addRule name op rules =
    rules |> Dict.insert name op

start : Operator -> Adapter o -> Parser o
start op adapter =
    let
        justStartRule = (noRules |> addRule "start" op)
    in
        withRules justStartRule adapter

-- OPERATORS

ch : Operator
ch =
    NextChar

match : String -> Operator
match subject =
    Match subject

choice : List Operator -> Operator
choice operators =
    Choice operators

seqnc : List Operator -> Operator
seqnc operators =
    Sequence operators

maybe : Operator -> Operator
maybe operator =
    Maybe_ operator

text : Operator -> Operator
text operator =
    TextOf operator

any : Operator -> Operator
any operator =
    Any operator

some : Operator -> Operator
some operator =
    Some operator

-- OPERATORS EXECUTION

execute : Operator -> Context o -> OperatorResult o
execute op ctx =
    ctx |> case op of
        NextChar -> execNextChar -- `ch`
        Match str -> execMatch str -- `match`
        Choice ops -> execChoice ops -- `choice`
        Sequence ops -> execSequence ops -- `seqnc`
        Maybe_ op -> execMaybe op -- `maybe`
        TextOf op -> execTextOf op -- `text`
        Any op -> execAny op -- `any`
        Some op -> execSome op -- `some`
        _ -> notImplemented

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

-- FIXME: http://folkertdev.nl/blog/loops-to-folds/

execChoice : List Operator -> Context o -> OperatorResult o
execChoice ops ctx =
    let
        reducedReport =
            List.foldl
                (\op prevStep ->
                    let
                        ( maybeSucceededBefore, prevFailures, prevCtx ) = prevStep
                    in
                        case maybeSucceededBefore of
                            Just _ -> prevStep
                            Nothing ->
                                let
                                    execResult = (execute op prevCtx)
                                    ( parseResult, newCtx ) = execResult
                                in
                                    case parseResult of
                                        Matched _ -> ( Just parseResult, prevFailures, newCtx )
                                        _ -> ( Nothing, prevFailures ++ [ parseResult ], newCtx )
                )
                ( Nothing, [], ctx )
                ops
        ( maybeChoiceSucceeded, failures, lastCtx ) = reducedReport
    in
        case maybeChoiceSucceeded of
            Just (Matched success) -> ( Matched success, lastCtx )
            _ -> ctx |> failedNestedCC failures

execSequence : List Operator -> Context o -> OperatorResult o
execSequence ops ctx =
    let
        reducedReport =
            List.foldl
                (\op prevStep ->
                    let
                        ( maybeFailedBefore, prevMatches, prevCtx ) = prevStep
                    in
                        case maybeFailedBefore of
                            Just _ -> prevStep
                            _ ->
                                let
                                    execResult = (execute op prevCtx)
                                    ( parseResult, newCtx ) = execResult
                                in
                                    case parseResult of
                                        Matched v -> ( Nothing, prevMatches ++ [ v ], newCtx )
                                        _ -> ( Just parseResult, prevMatches, newCtx )
                )
                ( Nothing, [], ctx )
                ops
        ( maybeSequenceFailed, matches, lastCtx ) = reducedReport
    in
        case maybeSequenceFailed of
            Just (Failed reason) -> ( Failed reason, ctx )
            _ -> lastCtx |> matchedList matches

execMaybe : Operator -> Context o -> OperatorResult o
execMaybe op ctx =
    let
        result = execute op ctx
    in
        case result of
            ( Matched s, newCtx ) -> matchedWith s newCtx
            _ -> matched "" ctx

execTextOf : Operator -> Context o -> OperatorResult o
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

execAny : Operator -> Context o -> OperatorResult o
execAny op ctx =
    let
        prevPos = ctx.position
        unfold = (\op ctx ->
            let
                ( mayBeMatched, nextCtx ) = ( execute op ctx )
            in
                case mayBeMatched of
                    Matched v -> [ ( v, nextCtx ) ] ++ (unfold op nextCtx)
                    _ -> []
            )
        result = unfold op ctx
    in
        case List.head result of
            Just ( v, lastCtx ) -> lastCtx |> matchedList
                                                (List.map Tuple.first result)
            Nothing -> ctx |> matched ""

execSome : Operator -> Context o -> OperatorResult o
execSome op ctx =
    let
        ( onceResult, nextCtx ) = (execute op ctx)
    in
        case onceResult of
            Matched v ->
                let
                    ( anyResult, lastCtx ) = (execAny op nextCtx)
                in
                    combine onceResult anyResult lastCtx
            failure -> ( failure, ctx )

-- UTILS

noValues : Values v
noValues = Dict.empty

initContext : Adapter o -> String -> Context o
initContext adapter input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , rules = noRules
    , values = noValues
    , adapt = adapter
    }

getStartRule : Parser o -> Maybe Operator
getStartRule parser =
    Dict.get "start" parser.rules

isNotParsed : ParseResult o -> Bool
isNotParsed result =
    case result of
        Matched _ -> False
        _ -> True

isParsedAs : String -> ParseResult o -> Bool
isParsedAs subject result =
    case result of
        Matched s -> (toString s == subject)
        _ -> False

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
    ctx |> failed (FollowingNestedRule ( failures, sample ))

failedNestedCC : List (ParseResult o) -> Context o -> OperatorResult o
failedNestedCC failures ctx =
    ctx |> failedNested failures (gotChar ctx)

notImplemented : Context o -> OperatorResult o
notImplemented ctx =
    ctx |> failed SomethingWasNotImplemented

-- failWith : Expectation -> Sample -> ParseResult
-- failWith expectation sample =
--     ExpectationFailure ( expectation, sample )

opResultToMaybe : OperatorResult o -> ( Maybe o, Context o )
opResultToMaybe ( parseResult, ctx ) =
    ( parseResultToMaybe parseResult, ctx )

parseResultToMaybe : ParseResult o -> Maybe o
parseResultToMaybe result =
    case result of
        Matched v -> Just v
        _ -> Nothing

combine : ParseResult o -> ParseResult o -> Context o -> OperatorResult o
combine resultOne resultTwo inContext =
    case ( resultOne, resultTwo ) of
        ( Matched vOne, Matched vTwo ) -> matchedList [ vOne, vTwo ] inContext
        _ -> ( resultTwo, inContext )

-- parseResultToMaybeInv : ParseResult o -> Maybe o
