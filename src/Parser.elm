module Parser exposing
    ( Parser, init, start, startWith, parse
    , Position, ParseResult(..), FailureReason(..), Expectation(..), Sample(..)
    , withRules, setStartRule, getStartRule, getRule, noRules, RuleName, Rules, RulesList
    , ch, match, choice, seqnc, maybe, text, any, some, and, not
    , action, pre, xpre, label, call, re, redesc
    , ActionResult(..), PrefixActionResult(..)
    , InputType(..)
    , Adapter
    , Operator(..), State
    )

{-|

# Parsing

If you just want to define some Rules and parse a text with them, then instantiate the [`BasicParser`](TODO)—this is the way to do your parsing fast and easy.

    import BasicParser.Parser as BasicParser exposing (..)
    import BasicParser.Export as Export exposing (..)
    import Parser exposing (..)

    let
        myParser = BasicParser.start
            <| choice
                [ match "foo"
                , match "bar"
                ]
    in
        myParser
            |> Parser.parse "foo"
            |> Export.parseResult
        {- Matched [ "foo" ] -}

        myParser
            |> Parser.parse "bar"
            |> Export.parseResult
        {- Matched [ "bar" ] -}

The `BasicParser` only knows how to operate with `String`s, but that should be enough for almost every parsing you would want. If you need more, read this sections and then advance to [Custom Parsers](#custom-parsers) section.

To explore more examples than this documentation has, see [sample parsers in the repository](https://github.com/shamansir/elm-friendly-parser/blob/master/test/samples).

[`Parser.start`](TODO) uses the provided [Operator tree](#Operators) as a Start Rule (the one executed first) and when you call `Parser.parse`, it applies the [Operators](#Operators) from the Start Rule to the input text in the same way they go:

    * `choice` means it should try variants passed inside one by one and when one passes, consider it as a success;
    * `match "foo"` means it should just try to match the string "foo" at current position;
    * `match "bar"` means it should just try to match the string "bar" at current position;
    * check if the input is parsed till the end and succeed if there were no failures before;

As you probably mentioned, the Rule may start only with one Operator, but then it may branch in the infinite directions, including the ability to call other Rules by name (we'll cover it later). If you need to start with a sequence of checks at the root point, just use `seqnc` (short for _sequence_) to wrap them.

To define your own Rules, you'll need [Operators](#Operators), such as `choice`,
`seqnc` (short for _sequence_) or `match`. Actually, all these Operators are inspired with [PEG Grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar) and every rule has the equivalent there, with several extensions. The ones we have out of the box are:

    * `match String`: match the given string;
    * `ch` : match exactly one character, no matter which;
    * `re String`: match the given regular expression;
    * `call String`: call the Rule by its name (we'll cover it below);
    * `seqnc (List Operator)`: perform the listed operators one by one;
    * `choice (List Operator)`: try the listed operators one by one, unless one matches;
    * `maybe Operator`: try to perform the given operator and continue even if it fails;
    * `any Operator`: try to perform the given operator several times and continue even if it fails;
    * `some Operator`: try to perform the given operator several times and continue only if matched at least one time;
    * `and Operator`: require the given operator to match, but do not advance the position after that;
    * `not Operator`: require the given operator not to match, but do not advance the position after that;
    * `action Operator UserCode`: execute the operator, then execute [the user code](#Actions) to let user determine if it actually matches, and also return any value user returned from the code;
    * `pre Operator UserPrefixCode`: execute the operator, then execute [the user code](#Actions) to let user determine if it actually matches, do not advance the position after that;
    * `xpre Operator UserPrefixCode`: execute the operator, then execute [the user code](#Actions) and match only if the code failed, do not advance the position after that;
    * `text Operator`: execute the operator, omit the results returned from the inside and return only the matched text as a string;
    * `label String Operator`: save the result of the given Operator in context under the given label, this is useful for getting access to this value from [user code](#Actions);

For the details on every Operator, see the [Operators](#Operators) section below.

`Export.parseResult` builds a friendly string from the [Parse Result](#parse-result), returned from `Parser.parse`.

[Parse Result](#parse-result) could be a complex structure, since it defines all the details it may get about the match or the failure, but in general it gets down to two variants:

    * `Matched value` — when parsing was successful;
    * `Failed failureReason` — when parsing was not successful;

For now, `Parser.parse` actually returns the pair of `(ParseResult, Maybe Position)` and this pair has the `position` (which is a tuple, `(Int, Int)`, with line index and character index) defined only on failure.

There is no requirement to have only one Rule, you may have dozens of them and you may call any by its name with [`call` Operator](TODO),but only one Rule may trigger the parsing process: The Start Rule. To build your own set of rules, not just a Start Rule, you'll need some other [initialization](#Initialization) methods:

For example, [`Parser.withRules`](TODO) allows you to define all the rules as a list:

    BasicParser.withRules
        [ ( "syllable", seqnc [ ch, ch ] )
        , ( "EOL", re "\n" )
        , ( "three-syllables",
            seqnc (List.repeat 3 (call "syllable")) )
        , ( "five-syllables",
            seqnc (List.repeat 5 (call "syllable")) )
        , ( "haiku",
            seqnc
                [ call "three-syllables", call "EOL"
                , call "five-syllables", call "EOL"
                , call "three-syllables", call "EOL"
                ] )
        ]
        |> Parser.setStartRule "haiku"
        |> Parser.parse "..."

Or, you call your rule in the Start Rule:

    BasicParser.withRules
        [ ( "syllable", seqnc [ ch, ch ] )
        , ...
        , ( "haiku", ... )
        ]
        |> Parser.startWith (call "haiku")
        |> Parser.parse "..."

Which is the same as:

    BasicParser.withRules
        [ ( "syllable", seqnc [ ch, ch ] )
        , ...
        , ( "haiku", ... )
        , ( "start", call "haiku" )
        ]
        |> Parser.parse "..."


So, if your Rule list contains a Rule under the name "start", it will be automatically called first.

This Parser implementation was inspired with the [functional version of `peg-js`](http://shamansir.github.io/blog/articles/generating-functional-parsers/) I made few years ago.

# Actions

Actions are the functions allowed to be executed when any [Operator](#Operators) was performed and they are allowed to replace its result with some value and/or report the success or failure of this Operator instead of the actual things happened in the process.

[`UserCode`](TODO) is the alias for a function `(o -> State o -> (ActionResult o))`.
[`UserPrefixCode`](TODO) is the alias for a function `(State o -> PrefixActionResult)`.

# Custom Parsers

NB: If you need to parse some string just now or define the rules for later use,
head to `[BasicParser](TODO)` instead. However, notice that the [Operators](#Operators) are stored in this module.

This module contains the definition of generic `Parser`, intended to be extended and / or customized using type variables. In this module, the `o` variable defines the user's `ReturnType`, as opposed to `InputType`.

`ReturnType` a.k.a. `o` (for `output`) is any type user wants to be returned from Parser [actions](#Actions).

For example, `BasicParser` is defined as:

    type alias BasicParser = Parser BasicParserReturnType

hence it returns its own type (which is `RString String | RList (List ReturnType) | RRule RuleName ReturnType`, very simple one) from all the actions and stores it in the actions and in the matches.

The `PhoneNumberParser` from [the samples](https://github.com/shamansir/elm-friendly-parser/blob/master/test/samples) is defined as:

    type alias PhoneNumberParserReturnType = String
    type alias PhoneNumberParser = Parser PhoneNumberParserReturnType

so it just returns `String` no matter what. However, the `TypedPhoneNumberParser` is defined as:

    type alias TypedPhoneNumberParser = Parser PhoneNumberPart

where `PhoneNumberPart` is:

    type PhoneNumberPart =
          AString String
        | AList (List PhoneNumberPart)
        | Prefix String Int
        | Operator Int
        | Local (Int, Int, Int)
        | PhoneNumber
            { prefix: (String, Int)
            , operator: Int
            , local: (Int, Int, Int)
            }

so it may define the phone number completely using a suggested type or fallback to `String` or `List` when some part of the phone number failed to match. This may happen even when parsing process was successful in general, for example it's allowed to fail to parse optional branches of the Operators `choice`, `maybe`, `any`, `some`, `not`.

If you want to create a custom parser on your own, you should consider which `ReturnType` you want to have and define the `Adapter` — the function which converts the `InputType` instances, received by the `Parser` during the general process of parsing (`String`, `List String` or a `Rule name`), to `o` a.k.a. the `ReturnType`, a type defining the final or a temporary result of your custom parsing process, of any complexity.

So, for the parser of Poker Game Hands, your `ReturnType` may define suit and a rank of every card. The parser of geographical definitions, such as KML files, may define `ReturnType` as a list of langitudes and longitudes and so on.

TODO!

# Initialization

@docs Parser
    , init
    , start
    , startWith

# Parsing

@docs parse

# Parse Result

@docs Position
    , ParseResult
    , FailureReason
    , Expectation
    , Sample

# Rules

@docs withRules
    , setStartRule
    , getStartRule
    , getRule
    , noRules
    , RuleName
    , Rules
    , RulesList

# Operators

@docs match
    , ch
    , re
    , redesc
    , call
    , seqnc
    , choice
    , maybe
    , any
    , some
    , and
    , not
    , action
    , pre
    , xpre
    , text
    , label

# Actions

@docs ActionResult
    , PrefixActionResult

# Custom Parser Requirements

@docs InputType
    , Adapter

# Operator and State

@docs Operator
    , State

# PEG compatibility and Export

-}

import Dict exposing (..)
import Regex

{-| When chunk was found in the input, it is stored in the `InputType`. When some sequence
is enclosed into another sequence and matched, the results are stored in the list. When the rule
matched, we need to store the name of the rule, so it's also stored.
-}
type InputType o =
      AValue String
    | AList (List o)
    | ARule RuleName o

{-| A custom user function which specifies for every Parser the converter from Source Type
to the Resulting Type (`o`). TODO
-}
type alias Adapter o = (InputType o -> o)

{-| TODO -}
type alias RuleName = String
{-| TODO -}
type alias Rules o = Dict RuleName (Operator o)
{-| TODO -}
-- FIXME: Rename to Grammar?
type alias RulesList o = List ( RuleName, Operator o )

{-| TODO -}
type alias Parser o =
    { adapt: Adapter o
    , rules: Rules o
    , startRule: String
    }

{-| TODO -}
init : Adapter o -> Parser o
init adapter =
    { adapt = adapter
    , rules = noRules
    , startRule = "start"
    }

type alias Values o = Dict String o

{-| TODO -}
type alias State o =
    { input: String
    , inputLength: Int
    , position: Int
    , values: Values o
}

type alias Context o = ( Parser o, State o )

initState : String -> State o
initState input =
    { input = input
    , inputLength = String.length input
    , position = 0
    , values = noValues
    }

noValues : Values v
noValues = Dict.empty

{-| TODO -}
-- FIXME: change ParseResult to some type which returns Matched | Failed (FailureReason, Position)
--        may be change ParseResult to `OpParseResult or OpSuccess = OpMatched | OpFailed` and keep --        it private.
--        Fix the docs in the intro then.
parse : String -> Parser o -> ( ParseResult o, Maybe Position )
parse input parser =
    let
        state = (initState input)
        context = (parser, state)
    in
        case getStartRule parser of
            Just startOperator ->
                -- TODO: extractParseResult (execCall parser.startRule context)
                let
                    ( parseResult, lastCtx ) = (execute startOperator context)
                    ( _, lastState ) = lastCtx
                in
                    case parseResult of
                        Matched success ->
                            if lastState.position == (String.length input) then
                                ( parseResult, Nothing )
                            else
                                ( Failed (ByExpectation
                                    (ExpectedEndOfInput, (GotValue (getCurrentChar lastCtx))))
                                , Just (findPosition lastState)
                                )
                        Failed _ -> ( parseResult, Just (findPosition lastState) )
            Nothing -> ( Failed NoStartRule, Nothing )

{-| TODO -}
noRules : Rules o
noRules = Dict.empty

{-| TODO -}
withRules : RulesList o -> Parser o -> Parser o
withRules rules parser =
    { parser | rules = Dict.fromList rules }
    -- , startRule = case List.head rules of
    --     Just ( name, _ ) -> name
    --     Nothing -> "start"

{-| TODO -}
start : Operator o -> Adapter o -> Parser o
start op adapter =
    init adapter |> startWith op

{-| TODO -}
startWith : Operator o -> Parser o -> Parser o
startWith op parser =
    parser |> addRule "start" op

addStartRule : Operator o -> Parser o -> Parser o
addStartRule = startWith

{-| TODO -}
getStartRule : Parser o -> Maybe (Operator o)
getStartRule parser =
    Dict.get parser.startRule parser.rules

{-| TODO -}
setStartRule : RuleName -> Parser o -> Parser o
setStartRule name parser =
    { parser | startRule = name }

addRule : RuleName -> Operator o -> Parser o -> Parser o
addRule name op parser =
    { parser | rules = parser.rules |> Dict.insert name op }

{-| TODO -}
getRule : RuleName -> Parser o -> Maybe (Operator o)
getRule name parser =
    Dict.get name parser.rules

{-| TODO -}
type ActionResult o = Pass o | PassThrough | Fail -- Return o | PassThrough | Fail
{-| TODO -}
type PrefixActionResult = Continue | Halt -- Continue | Stop (change ChainStep name to End or Exit/ExitWith)

type alias OperatorResult o = ( ParseResult o, Context o )

type alias UserCode o = (o -> State o -> (ActionResult o))
type alias UserPrefixCode o = (State o -> PrefixActionResult)

{-| TODO -}
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

{-| TODO -}
type Expectation =
      ExpectedValue String -- FIXME: InputType?
    | ExpectedAnything
    | ExpectedRuleDefinition RuleName
    | ExpectedRegexMatch String
    --| ExpectedStartRule
    | ExpectedEndOfInput

{-| TODO -}
type Sample =
      GotValue String
    | GotEndOfInput

{-| TODO -}
type FailureReason o =
      ByExpectation ( Expectation, Sample )
    | FollowingRule RuleName (FailureReason o)
    | FollowingNestedOperator ( List (FailureReason o), Sample )
    | NoStartRule
    | SomethingWasNotImplemented

{-| TODO -}
type alias Position = ( Int, Int )

{-| TODO -}
type ParseResult o =
      Matched o
    | Failed (FailureReason o)

-- OPERATORS

{-| TODO -}
ch : Operator o
ch =
    NextChar

{-| TODO -}
match : String -> Operator o
match subject =
    Match subject

{-| TODO -}
choice : List (Operator o) -> Operator o
choice operators =
    Choice operators

{-| TODO -}
seqnc : List (Operator o) -> Operator o
seqnc operators =
    Sequence operators

{-| TODO -}
maybe : Operator o -> Operator o
maybe operator =
    Maybe_ operator

{-| TODO -}
text : Operator o -> Operator o
text operator =
    TextOf operator

{-| TODO -}
any : Operator o -> Operator o
any operator =
    Any operator

{-| TODO -}
some : Operator o -> Operator o
some operator =
    Some operator

{-| TODO -}
and : Operator o -> Operator o
and operator =
    And operator

{-| TODO -}
not : Operator o -> Operator o
not operator =
    Not operator

{-| TODO -}
action : Operator o -> UserCode o -> Operator o
action operator userCode =
    Action operator userCode

{-| TODO -}
pre : UserPrefixCode o -> Operator o
pre userCode =
    PreExec userCode

{-| TODO -}
xpre : UserPrefixCode o -> Operator o
xpre userCode =
    NegPreExec userCode

{-| TODO -}
label : String -> Operator o -> Operator o
label name operator =
    Label name operator

{-| TODO -}
call : RuleName -> Operator o
call ruleName =
    Call ruleName

{-| TODO -}
re : String -> Operator o
re regex_ =
    Regex regex_ Nothing

{-| TODO -}
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
    let
        ( _, state ) = ctx
    in
        if (state.position >= state.inputLength) then
            ctx |> failedBy ExpectedAnything GotEndOfInput
        else
            ctx |> advanceBy 1 |> matched (getNextChar ctx)

execMatch : String -> Context o -> OperatorResult o
execMatch expectation ctx =
    let
        ( _, state ) = ctx
        inputLength = state.inputLength
        expectationLength = String.length expectation
    in
        if (state.position + expectationLength) > inputLength then
            ctx |> failedBy (ExpectedValue expectation) GotEndOfInput
        else
            if (String.startsWith expectation
                (state.input |> String.dropLeft state.position)) then
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
                    ( _, Nothing, matches, lastCtx ) ->
                        lastCtx |> matchedList matches
                    ( _, Just reason, failures, lastCtx ) ->
                        ctx |> loadPosition lastCtx |> failed reason

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
                        -- ctx |> loadPosition lastCtx |> matchedWith success
                    ( _, Nothing, failures ) ->
                        ctx |> failedNestedCC (keepOnlyFailures failures)

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
            ( Failed _, _ ) -> ctx |> matchedList []

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
        ( _, state ) = ctx
        prevPos = state.position
        result = execute op ctx
    in
        case result of
            ( Matched s, newCtx ) ->
                let
                    ( _, newState ) = newCtx
                in
                    newCtx |> matched
                        (newState.input |> String.slice prevPos newState.position)
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
            Matched _ -> ctx |> failedCC ExpectedEndOfInput
            failure -> matched "" ctx

execAction : Operator o -> UserCode o -> Context o -> OperatorResult o
execAction op userCode ctx =
    let
        ( result, newCtx ) = (execute op ctx)
        ( _, newState ) = newCtx
         -- we forget all the data left inside the "closure" and take only the new position
        resultingCtx = ctx |> loadPosition newCtx
    in
        case result of
            Matched v ->
                case (userCode v newState) of
                    Pass userV -> resultingCtx |> matchedWith userV
                    PassThrough -> resultingCtx |> matchedWith v
                    Fail -> resultingCtx |> failedCC ExpectedAnything
            Failed _ -> ( result, resultingCtx )

execPre : UserPrefixCode o -> Context o -> OperatorResult o
execPre userCode ctx =
    let
        ( _, state ) = ctx
        result = (userCode state)
    in
        case result of
            Continue -> ctx |> matched ""
            Halt -> ctx |> failedCC ExpectedEndOfInput

execNegPre : UserPrefixCode o -> Context o -> OperatorResult o
execNegPre userCode ctx =
    let
        ( _, state ) = ctx
        result = (userCode state)
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
                    let
                        ( parser, newState ) = newCtx
                        updatedState =
                            { newState | values = newState.values |> Dict.insert name v }
                    in
                        ( parser, updatedState )
                Failed _ -> newCtx
    in
        ( result, updatedCtx )

execCall : RuleName -> Context o -> OperatorResult o
execCall ruleName ctx =
    execCallAs ruleName ruleName ctx

execCallAs : RuleName -> RuleName -> Context o -> OperatorResult o
execCallAs ruleAlias realRuleName ctx =
    let
        ( parser, _ ) = ctx
    in
        case (getRule realRuleName parser) of
            Just op -> (execute op ctx) |> addRuleToResult ruleAlias
            Nothing -> ctx |> failedBy (ExpectedRuleDefinition realRuleName) (gotChar ctx)

-- execDefineRule : RuleName -> Operator o -> Context o -> OperatorResult o
-- execDefineRule ruleName op ctx =
--     matched "" { ctx | rules = ctx.rules |> addRule_ ruleName op }

execRegex : String -> Maybe String -> Context o -> OperatorResult o
execRegex regex maybeDesc ctx =
    -- FIXME: cache all regular expressions with Regex.Regex instances
    let
        ( _, state ) = ctx
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
                    ctx
                        |> advanceBy (String.length match.match)
                        |> matched match.match
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


advanceBy : Int -> Context o -> Context o
advanceBy count ctx =
    let
        ( parser, state ) = ctx
    in
        ( parser
        , { state | position = state.position + count }
        )

getNextSubstring : Context o -> Int -> Int -> String
getNextSubstring ctx shift count =
    let
        ( _, state ) = ctx
    in
        String.slice (state.position + shift) (state.position + shift + count) state.input

getNextChar : Context o -> String
getNextChar ctx =
    getNextSubstring ctx 1 1

getCurrentChar : Context o -> String
getCurrentChar ctx =
    getNextSubstring ctx 0 1

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
    let
        ( parser, _ ) = ctx
    in
        matchedWith (parser.adapt (AValue val)) ctx

matchedList : List o -> Context o -> OperatorResult o
matchedList val ctx =
    let
        ( parser, _ ) = ctx
    in
        matchedWith (parser.adapt (AList val)) ctx

matchedRule : RuleName -> o -> Context o -> OperatorResult o
matchedRule ruleName value ctx =
    let
        ( parser, _ ) = ctx
    in
        matchedWith (parser.adapt (ARule ruleName value)) ctx

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

failedNested : List (FailureReason o) -> Sample -> Context o -> OperatorResult o
failedNested failures sample ctx =
    ctx |> failed (FollowingNestedOperator ( failures, sample ))

failedNestedCC : List (FailureReason o) -> Context o -> OperatorResult o
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

loadPosition : Context o -> Context o -> Context o
loadPosition ( _, loadFrom ) ( parser, addTo ) =
    ( parser, { addTo | position = loadFrom.position } )

findPosition : State o -> Position
findPosition state =
    let
        input = state.input
        allLines = String.lines input
        linesCount = List.length allLines
        curPosition = (state.position - (linesCount - 1)) -- '\n' count as separate symbols
    in
        .cursor
            (List.foldl
                (\line { cursor, prevCursor, sum } ->
                    if (sum >= curPosition) then
                        { cursor = prevCursor
                        , prevCursor = prevCursor
                        , sum = sum
                        }
                    else
                        case cursor of
                            ( lineIndex, charIndex ) ->
                                let
                                    strlen = (String.length line)
                                in
                                    if (sum + strlen) > curPosition then
                                        { cursor = ( lineIndex, curPosition - sum )
                                        , prevCursor = cursor
                                        , sum = sum + strlen
                                        }
                                    else
                                        { cursor = ( lineIndex + 1, 0 )
                                        , prevCursor = cursor
                                        , sum = sum + strlen
                                        }
                )
                { cursor = (0, 0)
                , prevCursor = (0, 0)
                , sum = 0
                }
                (String.lines input))

keepOnlyMatches : List (ParseResult o) -> List o
keepOnlyMatches parseResults =
    List.filterMap
        (\result ->
            case result of
                Matched v -> Just v
                Failed _ -> Nothing)
        parseResults

keepOnlyFailures : List (ParseResult o) -> List (FailureReason o)
keepOnlyFailures parseResults =
    List.filterMap
        (\result ->
            case result of
                Matched _ -> Nothing
                Failed failure -> Just failure)
        parseResults
