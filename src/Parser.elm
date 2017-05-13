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

type alias Adapter i o = (i -> o)

type alias Parser i o = {
    adapter: Adapter i o,
    rules: Rules
}

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

type ParseResult o =
      Matched o
    | Failed ( Expectation o, Sample )
    | NoStartingRule
    | NotImplemented

-- type alias Context a = Dict String a
type alias Context i o =
    { input: String
    , inputLength: Int
    , position: Int
    , rules: Rules
    , values: Values o
    , adapter: Adapter i o
}

type alias OperatorResult i o = (ParseResult o, Context i o)

type alias Rules = Dict String Operator
type alias Values o = Dict String o

parse : Parser i o -> String -> ParseResult o
parse parser input =
    let
        ctx = (initContext parser.adapter input)
    in
        case getStartRule parser of
            Just startOperator ->
                extractParseResult (execute startOperator ctx)
            Nothing -> NoStartingRule

-- RULES

noRules : Rules
noRules = Dict.empty

addRule : String -> Operator -> Rules -> Rules
addRule name op rules =
    rules |> Dict.insert name op

withStartRule : Operator -> Adapter i o -> Parser i o
withStartRule op adapter =
    { adapter = adapter
    , rules = (noRules |> addRule "start" op)
    }

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

execute : Operator -> Context i o -> OperatorResult i o
execute op ctx =
    case op of
        NextChar -> execNextChar ctx
        Match str -> execMatch str ctx
        Choice ops -> execChoice ops ctx
        _ -> ( NotImplemented, ctx )

-- TODO: shortcuts for ( ExpectationFailure ..., ctx )
--       and ( Matched .., advanceBy ctx )

execNextChar : Context i o -> OperatorResult i o
execNextChar ctx =
    if (ctx.position >= ctx.inputLength) then
        ctx |> failed ExpectedAnything GotEndOfInput
    else
        ctx |> matchedAdvance (getNextChar ctx) 1

execMatch : String -> Context i o -> OperatorResult i o
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

execChoice : List Operator -> Context i o -> OperatorResult i o
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

execSequence : List Operator -> Context i o -> OperatorResult i o
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

initContext : Adapter i o -> String -> Context i o
initContext adapter input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , rules = noRules
    , values = noValues
    , adapter = adapter
    }

getStartRule : Parser a b -> Maybe Operator
getStartRule parser =
    Dict.get "start" parser.rules

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

advanceBy : Int -> Context i o -> Context i o
advanceBy cnt ctx =
    { ctx | position = ctx.position + cnt }

getNextChar : Context i o -> String
getNextChar ctx =
    String.slice (ctx.position + 1) (ctx.position + 2) ctx.input

getCurrentChar : Context i o -> String
getCurrentChar ctx =
    String.slice ctx.position (ctx.position + 1) ctx.input

gotChar : Context i o -> Sample
gotChar ctx =
    GotValue (getCurrentChar ctx)

extractParseResult : OperatorResult i o -> ParseResult o
extractParseResult opResult =
    Tuple.first opResult

extractContext : OperatorResult i o -> Context i o
extractContext opResult =
    Tuple.second opResult

matched : o -> Context i o -> OperatorResult i o
matched val ctx =
    ( Matched val, ctx )

matchedAdvance : o -> Int -> Context i o -> OperatorResult i o
matchedAdvance val count ctx =
    ( Matched val, ctx |> advanceBy count )

failed : Expectation o -> Sample -> Context i o -> OperatorResult i o
failed expectation sample ctx =
    ( Failed ( expectation, sample ), ctx )

-- fail with current character
failedCC : Expectation o -> Context i o -> OperatorResult i o
failedCC expectation ctx =
    ( Failed ( expectation, gotChar ctx ), ctx )

-- failWith : Expectation -> Sample -> ParseResult
-- failWith expectation sample =
--     ExpectationFailure ( expectation, sample )

-- TODO: add Adapter function to a Parser which will adapt the matching result to a single type
