module Parser exposing (..)

import Dict exposing (..)
import Utils exposing (..)

type alias UserCode = (ParseResult -> Operator)

type OperatorType =
      NextChar -- 1. `ch`
    | Match String -- 2. `match`
    | Regex String String -- 3. `re`
    | TextOf Operator -- 4. `text`
    | Maybe_ Operator -- 5. `maybe`
    | Some Operator -- 6. `some`
    | Any Operator  -- 7. `any`
    | And Operator -- 8. `and`
    | Not Operator -- 9. `not`
    | Sequence (List Operator) -- 10. `seqnc`
    | Choice (List Operator) -- 11. `choice`
    | Action Operator UserCode -- 12. `action`
    | PreExec UserCode -- 13. `pre`
    | NegPreExec UserCode -- 14. `xpre`
    | Label String Operator -- 15. `label`
    | Rule String Operator -- 16. `rule`
    | RuleReference String -- 17. `ref`
    | Alias String Operator -- 18. `as`

type alias Operator = OperatorType

type alias RuleName = String

-- type alias Chunk = ( Int, String )

type alias Parser = Rules

type Expectation v =
      ExpectedValue v
    | ExpectedAnything
    -- | ExpectedRule RuleName
    -- | ExpectedStartingRule
    -- | ExpectedOperator Operator
    | ExpectedEndOfInput
    -- | ExpectedChunk Chunk
    -- | ExpectedChunks (List Chunk)

type Sample =
      GotValue String
    | GotEndOfInput

type ParseResult v =
      Matched v
    | Failed ( Expectation v, Sample )
    | NoStartingRule
    | NotImplemented

-- type alias Context a = Dict String a
type alias Context v =
    { input: String
    , inputLength: Int
    , position: Int
    , rules: Rules
    , values: Values v
}

type alias OperatorResult v = (ParseResult v, Context v)

type alias Rules = Dict String Operator
type alias Values v = Dict String v

parse : Parser -> String -> ParseResult v
parse parser input =
    let
        ctx = (initContext input)
    in
        case getStartRule parser of
            Just startOperator ->
                extractParseResult (execute startOperator ctx )
            Nothing -> NoStartingRule

-- RULES

noRules : Rules
noRules = Dict.empty

addRule : String -> Operator -> Rules -> Rules
addRule name op rules =
    rules |> Dict.insert name op

start : Operator -> Rules
start op =
    noRules |> addRule "start" op

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

sequence : List Operator -> Operator
sequence operators =
    Sequence operators

-- OPERATORS EXECUTION

execute : Operator -> Context v -> OperatorResult v
execute op ctx =
    case op of
        NextChar -> execNextChar ctx
        Match str -> execMatch str ctx
        Choice ops -> execChoice ops ctx
        _ -> ( NotImplemented, ctx )

-- TODO: shortcuts for ( ExpectationFailure ..., ctx )
--       and ( Matched .., advanceBy ctx )

execNextChar : Context v -> OperatorResult v
execNextChar ctx =
    if (ctx.position >= ctx.inputLength) then
        ctx |> failed ExpectedAnything GotEndOfInput
    else
        ctx |> matchedAdvance (getNextChar ctx) 1

execMatch : String -> Context v -> OperatorResult v
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
                ctx |> failed (ExpectedValue expectation) GotEndOfInput

execChoice : List Operator -> Context v -> OperatorResult v
execChoice ops ctx =
    let
      maybeSuccess =
        Utils.iterateOr
            (\op ->
                let
                    execResult = (execute op ctx)
                in
                    case Tuple.first execResult of
                        Matched _ -> Just execResult
                        _ -> Nothing)
            ops
    in
        case maybeSuccess of
            Just value -> value
            Nothing -> ctx |> failedCC ExpectedAnything

execSequence : List Operator -> Context v -> OperatorResult v
execSequence ops ctx =
    let
      maybeSuccess =
        Utils.iterateMapAnd
            (\op ->
                let
                    execResult = (execute op ctx)
                in
                    case Tuple.first execResult of
                        Matched str -> Just str
                        _ -> Nothing)
            ops
    in
        case maybeSuccess of
            Just value -> ( Matched value, ctx )
            Nothing -> ctx |> failedCC ExpectedAnything

-- UTILS

noValues : Values v
noValues = Dict.empty

initContext : String -> Context v
initContext input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , rules = noRules
    , values = noValues
    }

getStartRule : Parser -> Maybe Operator
getStartRule parser =
    Dict.get "start" parser

isNotParsed : ParseResult v -> Bool
isNotParsed result =
    case result of
        Matched _ -> False
        _ -> True

isParsedAs : String -> ParseResult v -> Bool
isParsedAs subject result =
    case result of
        Matched s -> (toString s == subject)
        _ -> False

advanceBy : Int -> Context v -> Context v
advanceBy cnt ctx =
    { ctx | position = ctx.position + cnt }

getNextChar : Context v -> String
getNextChar ctx =
    String.slice (ctx.position + 1) (ctx.position + 2) ctx.input

getCurrentChar : Context v -> String
getCurrentChar ctx =
    String.slice ctx.position (ctx.position + 1) ctx.input

gotChar : Context v -> Sample
gotChar ctx =
    GotValue (getCurrentChar ctx)

extractParseResult : OperatorResult v -> ParseResult v
extractParseResult opResult =
    Tuple.first opResult

extractContext : OperatorResult v -> Context v
extractContext opResult =
    Tuple.second opResult

matched : v -> Context v -> OperatorResult v
matched val ctx =
    ( Matched val, ctx )

matchedAdvance : v -> Int -> Context v -> OperatorResult v
matchedAdvance val count ctx =
    ( Matched val, ctx |> advanceBy count )

failed : Expectation v -> Sample -> Context v -> OperatorResult v
failed expectation sample ctx =
    ( Failed ( expectation, sample ), ctx )

-- fail with current character
failedCC : Expectation v -> Context v -> OperatorResult v
failedCC expectation ctx =
    ( Failed ( expectation, gotChar ctx ), ctx )

-- failWith : Expectation -> Sample -> ParseResult
-- failWith expectation sample =
--     ExpectationFailure ( expectation, sample )

-- TODO: add Adapter function to a Parser which will adapt the matching result to a single type
