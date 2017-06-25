module Parser exposing
    ( Parser, init, start, startWith, parse
    , Position, ParseResult(..), FailureReason(..), Expectation(..), Sample(..)
    , withRules, setStartRule, getStartRule, getRule, noRules, RuleName, Rules, RulesList
    , ch, match, choice, seqnc, maybe, text, any, some, and, not
    , action, pre, xpre, label, call, re, redesc
    , ActionResult(..), PrefixActionResult(..), UserCode, UserPrefixCode
    , InputType(..)
    , Adapter
    , Operator(..), State
    )

{-| # Parsing

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

    1. `match String`: match the given string;
    2. `ch` : match exactly one character, no matter which;
    3. `re String`: match the given regular expression;
    4. `seqnc (List Operator)`: perform the listed operators one by one, also the way to nest things;
    5. `choice (List Operator)`: try the listed operators one by one, unless one matches;
    6. `maybe Operator`: try to perform the given operator and continue even if it fails;
    7. `any Operator`: try to perform the given operator several times and continue even if it fails;
    8. `some Operator`: try to perform the given operator several times and continue only if matched at least one time;
    9. `and Operator`: require the given operator to match, but do not advance the position after that;
    10. `not Operator`: require the given operator not to match, but do not advance the position after that;
    11. `call String`: call the Rule by its name (we'll cover it below);
    12. `action Operator UserCode`: execute the operator, then execute [the user code](#Actions) to let user determine if it actually matches, and also return any value user returned from the code;
    13. `pre Operator UserPrefixCode`: execute the operator, then execute [the user code](#Actions) to let user determine if it actually matches, do not advance the position after that;
    14. `xpre Operator UserPrefixCode`: execute the operator, then execute [the user code](#Actions) and match only if the code failed, do not advance the position after that;
    15. `text Operator`: execute the operator, omit the results returned from the inside and return only the matched text as a string;
    16. `label String Operator`: save the result of the given Operator in context under the given label, this is useful for getting access to this value from [user code](#Actions);

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

Actions are the functions allowed to be executed when any inner [Operator](#Operators) was performed and to replace its result with some value and/or report the success or failure of this Operator instead of the actual things happened during the process.

There are three operators designed explicitly to call actions: `action` itself, `pre` and `xpre`. Also there is one which cancels the effect of the inner actions: `text`. And the one, which allows you to save some value in context and reuse it later (but inside the same branch of operators): `label`.

    * [`UserCode`](TODO) is the alias for a function `ReturnType -> State -> ActionResult ReturnType`.
    * [`UserPrefixCode`](TODO) is the alias for a function `State -> PrefixActionResult`.

Let's see how you may change the flow of parsing with Actions:

TODO

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
    , seqnc
    , choice
    , maybe
    , any
    , some
    , and
    , not
    , call
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

import Core.Parser as P exposing (..)

{-| TODO -}
type alias Parser o = P.Parser o

-- INITIALIZATION

{-| TODO -}
init : Adapter o -> Parser o
init = P.init

{-| TODO -}
start : Operator o -> Adapter o -> Parser o
start = P.start

{-| TODO -}
startWith : Operator o -> Parser o -> Parser o
startWith = P.startWith

-- PARSING

{-| TODO -}
parse : String -> Parser o -> ( ParseResult o, Maybe Position )
parse = P.parse

-- PARSE RESULT

{-| TODO -}
type Position = P.Position

{-| TODO -}
type ParseResult o = P.ParseResult o

{-| TODO -}
type FailureReason o = P.FailureReason o

{-| TODO -}
type Expectation o = P.Expectation o

{-| TODO -}
type Sample o = P.Sample o

-- RULES

{-| TODO -}
withRules : RulesList o -> Parser o -> Parser o
withRules = P.withRules

{-| TODO -}
setStartRule : RuleName -> Parser o -> Parser o
setStartRule = P.setStartRule

{-| TODO -}
getStartRule : Parser o -> Maybe (Operator o)
getStartRule = P.getStartRule

{-| TODO -}
getRule : RuleName -> Parser o -> Maybe (Operator o)
getRule = P.getRule

{-| TODO -}
noRules : Rules o
noRules = P.noRules

{-| TODO -}
type RuleName = P.RuleName

{-| TODO -}
type Rules = P.Rules

{-| TODO -}
type RulesList = P.RulesList


-- OPERATORS


{-| This operator tries to match the next portion of an input with given string, using string length to consider the size of a portion to test. If the match passed, input position is advanced by the very same value. If input position plus string length exceeds input length – parser fails saying it reached end-of-input. If input does not contains the given string, parser fails saying current character and expected string. (It is possible to provide which part of input exactly was different, but original `peg.js` tests do not cover it and it's commonly considered optional, so it may be a homework for a reader).

* **Parser example:** `Parser.startWith <| seqnc [ ch, match "oo" ]`
* **PEG syntax:** `"<string>"`, `'<string>'`
* **PEG example:** `start = . 'oo'`

-}
match : String -> Operator o
match = P.match


{-| This operator hoists the next character from the text. If current position is greater than input length, it fails with telling that parser expected any symbol and got end-of-input instead. If next character is what we searched for, input position is advanced by one.

* **Parser example:** `Parser.startWith <| seqnc [ ch, ch, ch ]`
* **PEG syntax:** `.`
* **PEG example:** `start = . . .`

-}
ch : Operator o
ch = P.ch


{-| This operator tries to match using symbols-driven regular expression (the only allowed in `peg.js`). The regular expression may have some description provided, then this description will be used to describe a failure. On the other branches, this operator logic is similar to the one before.

* **Parser example:** `Parser.startWith <| some (re "[^f-o]")`
* **PEG syntax:** `[<symbols>]`, `[^<symbols>]`, `[<symbol_1>-<symbol_n>]`, `[^<symbol_1>-<symbol_n>]`, `"<string>"i`, `'<string>'i`
* **PEG example:** `start = [^f-o]+`


-}
-- FIXME: Pass regular expression options to `re`
re : String -> Operator o
re = P.re


{-| TODO -}
redesc : String -> String -> Operator o
redesc = P.redesc


{-| This operator executes a sequence of other operators of any kind, and this sequence may have any (but finite) length. If one of the given operators failed during execution, the sequence is interrupted immediately and the exception is thrown. If all operators performed with no errors, an array of their results is returned.

* **Parser example:** `Parser.startWith <| seqnc [ ch, match "oo", maybe (match "bar") ]`
* **PEG syntax:** `<expression_1> <expression_2> ...`
* **PEG example:** `start = . 'oo' 'bar'?`

 -}
seqnc : List (Operator o) -> Operator o
seqnc = P.seqnc


{-| This operator works similarly to pipe (`|`) operator in regular expressions – it tries to execute the given operators one by one, returning (actually, without advancing) the parsing position back in the end of each iteration.  If there was a success when one of these operators was executed, `choice` immediately exits with the successful result. If all operators failed, `choice` throws a `MatchFailed` exception.

* **Parser example:**
    `Parser.startWith <| seqnc`
    `    [ ch, choice [ match "aa", match "oo", match "ee" ], ch ]`
* **PEG syntax:** `<expression_1> / <expression_2> / ...`
* **PEG example:** `start = . ('aa' / 'oo' / 'ee') .`

 -}
choice : List (Operator o) -> Operator o
choice = P.choice


{-| This operator ensures that some other operator at least tried to be executed, but absorbs the failure if it happened. In other words, it makes other operator optional. `safe` function is the internal function to absorb operator failures and execute some callback if failure happened.


* **Parser example:**
    `Parser.startWith <|`
    `    seqnc [ maybe (match "f"),`
    `          , maybe (seqnc [ ch, ch ])`
    `          ]`
* **PEG syntax:** `<expression>?`
* **PEG example:** `start = 'f'? (. .)?`

-}
maybe : Operator o -> Operator o
maybe = P.maybe


{-| This operator executes other operator the most possible number of times, but even no matches at all will suffice as no failure. `any` operator also returns an array of matches, but the empty one if no matches succeeded.

* **Parser example:**
    `Parser.startWith <| seqnc [ some (match "f"), any (match "o") ]`
* **PEG syntax:** `<expression>*`
* **PEG example:** `start = 'f'+ 'o'*`

-}
any : Operator o -> Operator o
any = P.any


{-| This operator executes other operator the most possible number of times (but at least one) until it fails (without failing the parser). If it failed at the moment of a first call – then the whole parser failed. If same operator failed during any of the next calls, failure is absorbed without advancing parsing position further. This logic is often called "one or more" and works the same way in regular expressions. In our case, we achieve the effect by calling the operator itself normally and then combining it with immediately-called`any` ("zero or more") operator described just below.

`some` operator returns the array of matches on success, with at least one element inside.

* **Parser example:** `Parser.startWith <| seqnc [ maybe (match "f"), some ch ]`
* **PEG syntax:** `<expression>+`
* **PEG example:** `start = 'f'? .+`

-}
some : Operator o -> Operator o
some = P.some


{-| `and` operator executes other operator almost normally, but returns an empty string if it matched and failures expecting end-of-input if it failed. Also, everything happens without advancing the parser position. `pos` variable here is global parser position and it is rolled back after the execution of inner operator. `nr` flag is 'no-report' flag, it is used to skip storing parsing errors data (like their postions), or else they all stored in order of appearance, even if they don't lead to global parsing failure.

It's important to say here that, honestly speaking, yes, `peg.js-fn` is aldo driven by exceptions, among with postponed function. One special class of exception, named `MatchFailed`. It is raised on every local parse failure, but sometimes it is absorbed by operators wrapping it (i.e. `safe` function contains `try {...} catch(MatchFailed) {...}` inside), and sometimes their logic tranfers it to the top (global) level which causes the final global parse failure and parsing termination. The latter happens once and only once for every new input/parser execution, of course.

* **Parser example:** `Parser.startWith <| seqnc [ and (match "f"), match "foo" ]`
* **PEG syntax:** `&<expression>`
* **PEG example:** `start = &'f' 'foo'`

-}
and : Operator o -> Operator o
and = P.and

{-| `not` operator acts the same way as the `and` operator, but in a bit inverse manner. It also ensures not to advance the position, but returns an empty string when match failed and fails with expecting end-of-input, if match succeeded.

* **Parser example:** `Parser.startWith <| seqnc [ not (match "g"), match "foo" ]`
* **PEG syntax:** `!<expression>`
* **PEG example:** `start = !'g' 'foo'`

-}
not : Operator o -> Operator o
not = P.not

{-| This operator is different from others, because it just wraps a rule and calls its first wrapping operator immediately and nothing more. It only used to provide better readibility of parser code, so you (as well as parser itself) may link to any rule using `rules.<your_rule>` reference.

* **syntax:** `<rule_name> = <expression>`
* **example:**
    `space = " "`
    `foo "three symbols" = . . .`
    `start = !space foo !space`
* **code:**
    `rules.space = function() { return (match(' '))(); };`
    `rules.foo = function() { return (as('three symbols', seqnc(ch(), ch(), ch())))(); };`
    `rules.start = function() { return (seqnc(not(ref(rules.space)), ref(rules.foo), not(ref(rules.space))))(); };`

...And if we plan to call some rule from some operator with `rules.<rule_name>` reference, we need to make current context accessible from the inside. Context is those variables who accessible at this nesting level and above (nesting level is determined with brackets in grammar). This provided with some complex tricks, but we'll keep them for those who want to know all the details – if you're one of them, the next chapter is completely yours.

* **example:**
    `fo_rule = 'fo'`
    `start = fo_rule 'o'`
* **code:**
    `rules.fo_rule = function() { return (match('fo'))(); };`
    `rules.start = function() { return (seqnc(ref(rules.fo_rule), match('o'))(); };`

-}
call : RuleName -> Operator o
call = P.call

{-| 12. `action`

In `peg.js` any rule or sequence may have some javascript code assigned to it, so it will be executed on a successful match event, and in latter case this code has the ability to manipulate the match result it receives and to return the caller something completely different instead.

Commonly the operators which themselves execute some other, inner operators, (and weren't overriden) return the array containing their result values, if succeeded. Other operators return plain values. With `action`, both these types of results may be replaced with any crap developer will like.

By the way, the code also receives all the values returned from labelled operators (on the same nesting level and above) as the variables with the names equal to the labels. See more information on labelling below.

* **Parser example:**
    `Parser.startWith <| seqnc [ match "fo"`
    `    , action ch (\state _ -> Pass state.offset )`
* **PEG syntax:** `<expression> { <javascript-code> }`
* **PEG.js example:** `start = 'fo' (. { return offset(); })`

-}
action : Operator o -> UserCode o -> Operator o
action = P.action

{-| 13. `pre`

The rule in `peg.js` also may be prefixed/precessed with some JavaScript code which is executed before running all the inner rule operators. This JavaScript code may check some condition(s) and decide, if it's ever has sense to run this rule, with returning a boolean value. Of course, this code does not advances the parser position.

* **Parser example:** `Parser.startWith <| seqnc [ pre (\_ -> Continue), match "foo" ]`
* **PEG syntax:** `& { <javascript-code> }`
* **PEG.js example:** `start = &{ return true; } 'foo'`

-}
pre : UserPrefixCode o -> Operator o
pre = P.pre

{-| 14. `xpre`

Same as `pre` operator, but in this case, reversely, `false` returned says it's ok to execute the rule this operator precedes.

* **Parser example:** `Parser.startWith <| seqnc [ xpre (\_ -> Halt), match "foo" ]`
* **PEG.js syntax:** `! { <javascript-code> }`
* **PEG example:** `start = !{ return false; } 'foo'`

-}
xpre : UserPrefixCode o -> Operator o
xpre = P.xpre

{-| `text` operator executes the other operator inside as normally, but always returns the matched portion of input text instead of what the inner operator decided to return. If there will be failures during the inner operator parsing process, return code will not ever be reached.

* **Parser example:** `Parser.startWith <| text (seqnc [ ch, ch, ch ]);`
* **PEG syntax:** `$<expression>`
* **PEG example:** `start = $(. . .)`

-}
text : Operator o -> Operator o
text = P.text

{-| `label` operator allows to tag some expression with a name, which makes it's result to be accessible to the JavaScript code through variable having the exact same name. Since you may execute JavaScript code in the end of any sequence operator `sqnc` by wrapping it with `action` operator, you may get access to these values from everywhere, and only bothering if current nesting level has access to the label you want to use.

* **Parser example:** `Parser.startWith action(seqnc(label('a', ch()), match('oo')), function(a) { return a + 'bb'});`
* **PEG syntax:** `<name>:<expression>`
* **PEG example:** `start = a:. 'oo' { return a + 'bb'; }`

-}
label : String -> Operator o -> Operator o
label = P.label

{-
The final operator creates an alias for a rule so it will be referenced with another name in error messages. And it's the only purpose of this one, the last one.

* **syntax:** `<rule_name> "<alias>" = <expression>`
* **example:** `start "blah" = 'bar'`
* **code:** `rules.start = function() { return (as('blah', match('bar')))(); };`

-}

-- ACTIONS

{-| TODO -}
type ActionResult o = P.ActionResult o
{-| TODO -}
type PrefixActionResult = P.PrefixActionResult
{-| TODO -}
type UserCode o = P.UserCode o
{-| TODO -}
type UserPrefixCode = P.UserPrefixCode

-- CUSTOM PARSER REQUIREMENTS

{-| When chunk was found in the input, it is stored in the `InputType`. When some sequence
is enclosed into another sequence and matched, the results are stored in the list. When the rule
matched, we need to store the name of the rule, so it's also stored.
-}
type InputType o = P.InputType o

{-| A custom user function which specifies for every Parser the converter from Source Type
to the Resulting Type (`o`). TODO
-}
type Adapter o = P.Adapater o

-- OPERATOR AND STATE

{-| TODO -}
type Operator o = P.Operator o

{-|

* `input` – contains the string that was passed to a `parse()` function, so here it stays undefined and just provides global access to it, but surely it's initialized with new value on every call to `parse()`;
* `pos` – current parsing position in the `input` string, it resets to 0 on every `parse()` call and keeps unevenly increasing until reaches the length of current `input` minus one, except the cases when any of fall-back operators were met (like `choice` or  `and` or `pre` or `xpre` or ...), then it moves back a bit or stays at one place for some time, but still returns to increasing way just after that;
* `p_pos` (notice the underscore) – previous parsing position, a position in `input` string where parser resided just before the execution of current operator. So for matching operators (`match`, `ref`, `some`, `any`, ...), a string chunk between `input[p_pos]` and `input[pos]` is always a matched part of an input.
* `options` – options passed to `parse()` function;

-}
type State = P.State
