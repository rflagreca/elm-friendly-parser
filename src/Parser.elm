module Parser exposing (..)

import Dict exposing (..)
import Utils exposing (..)

type alias UserCode o = (o -> Context o -> (Maybe o))
type alias UserPrefixCode o = (Context o -> Bool)

type OperatorType o =
      NextChar -- 1. `ch` -- DONE
    | Match String -- 2. `match` -- DONE
    | Regex String String -- 3. `re`
    | TextOf (Operator o) -- 4. `text` -- DONE
    | Maybe_ (Operator o) -- 5. `maybe` -- DONE
    | Some (Operator o) -- 6. `some` -- 1/2 DONE
    | Any (Operator o)  -- 7. `any` -- DONE
    | And (Operator o) -- 8. `and` -- DONE
    | Not (Operator o) -- 9. `not` -- DONE
    | Sequence (List (Operator o)) -- 10. `seqnc` -- DONE
    | Choice (List (Operator o)) -- 11. `choice` -- DONE
    | Action (Operator o) (UserCode o) -- 12. `action` -- DONE
    | PreExec (UserPrefixCode o) -- 13. `pre` -- DONE
    | NegPreExec (UserPrefixCode o) -- 14. `xpre` -- DONE
    | Label String (Operator o) -- 15. `label` -- DONE
    -- | Rule RuleName (Operator o) -- 16. `rule`
    | Call RuleName -- 17. `call` a.k.a `ref`
    | Alias String (Operator o) -- 18. `as`

type alias Operator o = OperatorType o

type alias RuleName = String

type InputType o = AValue String | AList (List o)

-- type alias Chunk = ( Int, String )

type alias Adapter o = (InputType o -> o)

type alias Parser o = {
    adapt: Adapter o,
    rules: Rules o
}

type Expectation =
      ExpectedValue String -- FIXME: InputType?
    -- | ExpectedList (List String)
    | ExpectedAnything
    -- | ExpectedRule RuleName
    | ExpectedRuleDefinition RuleName
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

-- FIXME: Merge Parser and Context in a Parser record ??

-- type alias Context a = Dict String a
type alias Context o =
    { input: String
    , inputLength: Int
    , position: Int
--    , previousPosition: Int
    , rules: Rules o
    , values: Values o
    , adapt: Adapter o
}

type alias OperatorResult o = (ParseResult o, Context o)

type alias Rules o = Dict String (Operator o)
type alias RulesList o = List ( String, Operator o )
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

noRules : Rules o
noRules = Dict.empty

withRules : Rules o -> Adapter o -> Parser o
withRules rules adapter =
    { adapt = adapter
    , rules = rules
    }

-- TODO: withRules should accept RulesList instead

withListedRules : RulesList o -> Adapter o -> Parser o
withListedRules rulesList adapter =
    withRules (Dict.fromList rulesList) adapter

addRule : RuleName -> Operator o -> Parser o -> Parser o
addRule name op parser =
    { parser | rules = parser.rules |> addRule_ name op }

addRule_ : RuleName -> Operator o -> Rules o -> Rules o
addRule_ name op rules =
    rules |> Dict.insert name op

getRule : RuleName -> Parser o -> Maybe (Operator o)
getRule name parser =
    Dict.get name parser.rules

getRule_ : RuleName -> Context o -> Maybe (Operator o)
getRule_ name ctx =
    Dict.get name ctx.rules

start : Operator o -> Adapter o -> Parser o
start op adapter =
    let
        justStartRule = (noRules |> addRule_ "start" op)
    in
        withRules justStartRule adapter

startWith : Operator o -> Parser o -> Parser o
startWith op parser =
    parser |> addRule "start" op

addStartRule : Operator o -> Parser o -> Parser o
addStartRule = startWith

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

-- OPERATORS EXECUTION

execute : Operator o -> Context o -> OperatorResult o
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
        And op -> execAnd op -- `and`
        Not op -> execNot op -- `not`
        Action op uc -> execAction op uc -- `action`
        PreExec uc -> execPre uc -- `pre`
        NegPreExec uc -> execNegPre uc -- `xpre`
        Label n op -> execLabel n op -- `label`
        Call n -> execCall n -- `call` a.k.a. `ref`
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

execChoice : List (Operator o) -> Context o -> OperatorResult o
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

execSequence : List (Operator o) -> Context o -> OperatorResult o
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

execMaybe : Operator o -> Context o -> OperatorResult o
execMaybe op ctx =
    let
        result = execute op ctx
    in
        case result of
            ( Matched s, newCtx ) -> matchedWith s newCtx
            _ -> matched "" ctx

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

execAny : Operator o -> Context o -> OperatorResult o
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
            -- FIXME: store only the last context, not the context for every operation
            Just ( v, lastCtx ) -> matchedList (List.map Tuple.first result) lastCtx
            Nothing -> ctx |> matched ""

execSome : Operator o -> Context o -> OperatorResult o
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
                    Just userV -> ( Matched userV, newCtx )
                    Nothing -> newCtx |> failedCC ExpectedAnything
            _ -> ( result, newCtx )

execPre : UserPrefixCode o -> Context o -> OperatorResult o
execPre userCode ctx =
    let
        result = (userCode ctx)
    in
        case result of
            True -> ctx |> matched ""
            False -> ctx |> failedCC ExpectedEndOfInput

execNegPre : UserPrefixCode o -> Context o -> OperatorResult o
execNegPre userCode ctx =
    let
        result = (userCode ctx)
    in
        case result of
            True -> ctx |> failedCC ExpectedEndOfInput
            False -> ctx |> matched ""

execLabel : String -> Operator o -> Context o -> OperatorResult o
execLabel name op ctx =
    let
        ( result, newCtx ) = (execute op ctx)
        updatedCtx =
            case result of
                Matched v ->
                    { newCtx | values = newCtx.values |> Dict.insert name v }
                _ -> newCtx
    in
        ( result, updatedCtx )

execCall : RuleName -> Context o -> OperatorResult o
execCall ruleName ctx =
    case (getRule_ ruleName ctx) of
        Just op -> (execute op ctx)
        -- TODO: add Rule name to the Match and Failure information
        Nothing -> ctx |> failedBy (ExpectedRuleDefinition ruleName) (gotChar ctx)

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

getStartRule : Parser o -> Maybe (Operator o)
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
        ( Matched vOne, Matched vTwo ) ->
            matchedList [ vOne, vTwo ] inContext
        _ -> ( resultTwo, inContext )

-- parseResultToMaybeInv : ParseResult o -> Maybe o
