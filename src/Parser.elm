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

type alias Chunk = ( Int, String )

type alias Parser = Rules

type ParseResult =
      Matched String
    | ExpectedRule RuleName
    | ExpectedOperator Operator
    | ExpectedChunk Chunk
    | ExpectedChunks (List Chunk)
    | NoStartingRule

type alias Rules = Dict String Operator
type alias Values v = Dict String v

-- type alias Context a = Dict String a
type alias Context v = {
    position: Int,
    rules: Rules,
    values: Values v
}

parse : Parser -> String -> ParseResult
parse parser input =
    case getStartRule parser of
        Just startOp -> Matched input
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

match : String -> Operator
match subject =
    Match subject

choice : List Operator -> Operator
choice operators =
    Choice operators

-- OPERATORS EXECUTION

execute : Operator -> Context v -> String -> String
execute op ctx input =
    case op of
        Match s -> execMatch s input ctx
        _ -> input

execMatch : String -> String -> Context v -> String
execMatch expectation input ctx =
    input

-- UTILS

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
