module Parser exposing (..)

import Dict exposing (..)
import Utils exposing (..)

type alias UserCode = (ParseResult -> Operator)

type OperatorType =
      NextChar -- 1. `ch` -- DONE
    | Match String -- 2. `match` -- DONE
    | Regex String String -- 3. `re`
    | TextOf Operator -- 4. `text`
    | Maybe_ Operator -- 5. `maybe`
    | Some Operator -- 6. `some`
    | Any Operator  -- 7. `any`
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

type InputType o = AString String | AList (List o)

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

type ParseResult o =
      Matched o
    -- FIXME: | MatchedList ( List o ) // NB: replaced by `matchedList` using `ctx.adapt (AList a)`
    | Failed ( Expectation, Sample )
    | FailedNested ( List (ParseResult o), Sample )
    -- FIXME: | FailedNested ( List Expectation, Sample )
    -- FIXME: | Nested ( List (ParseResult o), Sample )
    | NoStartRule
    | NotImplemented

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
            Nothing -> NoStartRule

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

-- OPERATORS EXECUTION

execute : Operator -> Context o -> OperatorResult o
execute op ctx =
    case op of
        NextChar -> execNextChar ctx -- `ch`
        Match str -> execMatch str ctx -- `match`
        Choice ops -> execChoice ops ctx -- `choice`
        Sequence ops -> execSequence ops ctx -- `seqnc`
        Maybe_ op -> execMaybe op ctx -- `maybe`
        _ -> ( NotImplemented, ctx )

execNextChar : Context o -> OperatorResult o
execNextChar ctx =
    if (ctx.position >= ctx.inputLength) then
        ctx |> failed ExpectedAnything GotEndOfInput
    else
        ctx |> matchedAdvance (getNextChar ctx) 1

execMatch : String -> Context o -> OperatorResult o
execMatch expectation ctx =
    let
        inputLength = ctx.inputLength
        expectationLength = String.length expectation
    in
        if (ctx.position + expectationLength) > inputLength then
            ctx |> failed (ExpectedValue expectation) GotEndOfInput
        else
            if (String.startsWith expectation
                (ctx.input |> String.dropLeft ctx.position)) then
                ctx |> matchedAdvance expectation expectationLength
            else
                -- FIXME: return next char, not EOI
                ctx |> failed (ExpectedValue expectation) GotEndOfInput

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
            Just success -> ( success, lastCtx )
            Nothing -> ctx |> failedNestedCC failures

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
                            Nothing ->
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
        ( maybeSequencsFailed, matches, lastCtx ) = reducedReport
    in
        case maybeSequencsFailed of
            Just failure -> ctx |> failedCC ExpectedAnything
            Nothing -> ctx |> matchedList matches

execMaybe : Operator -> Context o -> OperatorResult o
execMaybe op ctx =
    let
        result = execute op ctx
    in
        case result of
            ( Matched s, newCtx ) -> matchedWith s newCtx
            _ -> matched "" ctx

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
    ( Matched (ctx.adapt (AString val)), ctx )

matchedList : List o -> Context o -> OperatorResult o
matchedList val ctx =
    ( Matched (ctx.adapt (AList val)), ctx )

matchedAdvance : String -> Int -> Context o -> OperatorResult o
matchedAdvance val count ctx =
    ( Matched (ctx.adapt (AString val)), ctx |> advanceBy count )

failed : Expectation -> Sample -> Context o -> OperatorResult o
failed expectation sample ctx =
    ( Failed ( expectation, sample ), ctx )

-- fail with current character
failedCC : Expectation -> Context o -> OperatorResult o
failedCC expectation ctx =
    ( Failed ( expectation, gotChar ctx ), ctx )

failedNested : List (ParseResult o) -> Sample -> Context o -> OperatorResult o
failedNested failures sample ctx =
    ( FailedNested ( failures, sample ), ctx )

failedNestedCC : List (ParseResult o) -> Context o -> OperatorResult o
failedNestedCC failures ctx =
    ( FailedNested ( failures, gotChar ctx ), ctx )

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
