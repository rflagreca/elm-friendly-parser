module Parser exposing (..)

import Dict exposing (..)

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

type Expectation =
      ExpectedValue String
    | ExpectedAnything
    | ExpectedRule RuleName
    -- | ExpectedOperator Operator
    | ExpectedEndOfInput
    -- | ExpectedChunk Chunk
    -- | ExpectedChunks (List Chunk)

type Sample =
      GotValue String
    | GotEndOfInput

type ParseResult =
      Matched String
    | ExpectationFailure ( Expectation, Sample )
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

type alias OperatorResult v = (ParseResult, Context v)

type alias Rules = Dict String Operator
type alias Values v = Dict String v

parse : Parser -> String -> ParseResult
parse parser input =
    case getStartRule parser of
        Just startOperator ->
            extractParseResult (execute startOperator (initContext input))
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

-- OPERATORS EXECUTION

execute : Operator -> Context v -> OperatorResult v
execute op ctx =
    case op of
        NextChar -> execNextChar ctx
        Match str -> execMatch str ctx
        _ -> ( NotImplemented, ctx )

execNextChar : Context v -> OperatorResult v
execNextChar ctx =
    if (ctx.position >= ctx.inputLength) then
        ( ExpectationFailure ( ExpectedAnything, GotEndOfInput )
        , ctx )
    else
        ( Matched (getNextChar ctx)
        , advanceBy 1 ctx )

execMatch : String -> Context v -> OperatorResult v
execMatch expectation ctx =
    let
        inputLength = ctx.inputLength
        expectationLength = String.length expectation
    in
        if (ctx.position + expectationLength) > inputLength then
            ( ExpectationFailure ( ExpectedValue expectation
                                 , GotEndOfInput )
            , ctx )
        else
            if (String.startsWith expectation
                (ctx.input |> String.dropLeft ctx.position)) then
                ( Matched expectation
                , advanceBy expectationLength ctx )
            else
                ( ExpectationFailure ( ExpectedValue expectation
                                     , GotValue (getCurrentChar ctx) )
                , ctx )

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

isNotParsed : ParseResult -> Bool
isNotParsed result =
    case result of
        Matched _ -> False
        _ -> True

isParsedAs : String -> ParseResult -> Bool
isParsedAs subject result =
    case result of
        Matched s -> (s == subject)
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

extractParseResult : OperatorResult v -> ParseResult
extractParseResult opResult =
    Tuple.first opResult

extractContext : OperatorResult v -> Context v
extractContext opResult =
    Tuple.second opResult
