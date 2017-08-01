module StringParserTest exposing (suite)

import Dict

import Test exposing (..)
import Expect

import Parser exposing (..)
import Grammar exposing (..)
import Operator exposing (..)
import Action exposing (..)
import ParseResult exposing (..)
import State exposing (Position)
import Match exposing (..)
import StringParser.Parser as StringParser exposing (..)

import Utils exposing (..)

suite : Test
suite =
    describe "basic friendly parser"
        [ testStartRule
        , testBasicMatching
        , testChoiceMatching
        , testSequenceMatching
        , testMaybeMatching
        , testTextMatching
        , testAnyMatching
        , testSomeMatching
        , testAndMatching
        , testNotMatching
        , testActionMatching
        , testPreMatching
        , testNegPreMatching
        , testLabelMatching
        , testREMatching
        , testDefiningAndCallingRules
        , testReportingPosition
        ]

-- (-<) : StringParser.Operator -> StringParser
-- (-<) op =
--     StringParser.use <| op

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            (StringParser.withRules []
                |> Parser.configure
                |> expectToFailToParseWith
                    "foo"
                    (Failed NoStartRule (0, 0)))
        -- FIXME: test allowing to specify custom startRule by name (below, in testDefiningAndCallingRules)
        ]

testBasicMatching : Test
testBasicMatching =
    describe "basic matching"
        [ test "matches simple string" <|
            ((StringParser.use <|
                (match "abc"))
                |> expectToParse
                    "abc"
                    "abc")
        , test "not matches a string when it is unequeal to the one expected" <|
            ((StringParser.use <| (match "abc"))
                |> expectToFailToParse "ab")
        , test "fails when input end wasn't reached" <|
            ((StringParser.use <|
                (match "ab"))
                |> expectToFailToParseWith
                    "abc"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "c")) (0, 2)))
        , test "reports the failed match properly" <|
            ((StringParser.use <|
                (match "foo"))
                |> expectToFailToParseWith
                    "for"
                    (Failed (ByExpectation (ExpectedValue "foo", GotValue "f")) (0, 0))) -- GotValue "for"
        -- FIXME: test fails when not the whole input matched
        ]

testChoiceMatching : Test
testChoiceMatching =
    describe "`choice` matching"
        [ test "matches correctly" <|
            let
                parser = StringParser.use <| choice [ match "a", match "b", match "c" ]
            in
                Expect.all
                    [ parser |> expectToParse "a" "a"
                    , parser |> expectToParse "b" "b"
                    , parser |> expectToParse "c" "c"
                    , parser |> expectToFailToParse "d"
                    ]
        , test "fails correctly" <|
            ((StringParser.use <|
                choice [ match "a", match "b", match "c" ])
                |> expectToFailToParseWith
                    "foo"
                    (nestedFailureOf
                        [ ( "a", (GotValue "f") )
                        , ( "b", (GotValue "f") )
                        , ( "c", (GotValue "f") )
                        ]
                        (GotValue "f")
                        (0, 0)))
        , test "gets first matching result" <|
            ((StringParser.use <|
                choice [ match "foo", match "f" ])
                |> expectToParse
                    "foo"
                    "foo")
        , test "gets first matching result in a chain" <|
            ((StringParser.use <|
                choice [ match "a", match "foo", match "f" ])
                |> expectToParse
                    "foo"
                    "foo")
        , test "properly advances position" <|
            ((StringParser.use <|
                getPositionAfter
                    (choice [ match "foo", match "bars" ]))
                |> expectToParse
                    "bars"
                    "4")
        ]

testSequenceMatching : Test
testSequenceMatching =
    describe "`seqnc` matching"
        [ test "matches correctly" <|
            ((StringParser.use <|
                seqnc [ match "f", match "o", match "o" ])
                |> expectToParseNested
                    "foo"
                    [ "f", "o", "o" ])
        , test "fails if one of the operators fails" <|
            ((StringParser.use <|
                seqnc [ match "f", match "o", match "p" ])
                |> expectToFailToParse "foo")
        , test "fails correctly" <|
            ((StringParser.use <|
                seqnc [ match "f", match "o", match "p" ])
                |> expectToFailToParseWith
                "foo"
                (Failed (ByExpectation (ExpectedValue "p", GotValue "o" )) (0, 2)))
        ]

testMaybeMatching : Test
testMaybeMatching =
    describe "`maybe` matching"
        [ test "matches when sample exists" <|
            ((StringParser.use <|
                seqnc [ match "f", match "o", maybe (match "o") ])
                |> expectToParseNested
                    "foo"
                    [ "f", "o", "o" ])
        , test "matches when sample not exists" <|
            ((StringParser.use <|
                seqnc [ match "f", match "o", maybe (match "o") ])
                |> expectToParseNested
                    "fo"
                    [ "f", "o", "" ])
        , test "matches when sample not exists, p. II" <|
            ((StringParser.use <|
                seqnc [ match "f", match "o", maybe (match "p"), match "o" ])
                |> expectToParseNested
                    "foo"
                    [ "f", "o", "", "o" ])
        ]

testTextMatching : Test
testTextMatching =
    describe "`text` matching"
        [ test "matches when sample exists" <|
            ((StringParser.use <|
                text (seqnc [ match "f", match "o", match "o" ]))
                |> expectToParse
                    "foo"
                    "foo")

        , test "still matches when a part of a sample not exists" <|
            ((StringParser.use <|
                text (seqnc [ match "f", match "o", maybe (match "o") ]))
                |> expectToParse
                    "fo"
                    "fo")
        , test "fails when nested operator is not matching" <|
            ((StringParser.use <|
                text ( seqnc [ match "f", match "o", match "o" ]))
                |> expectToFailToParseWith
                    "bar"
                    (Failed (ByExpectation (ExpectedValue "f", GotValue "b")) (0, 0)))
        ]

testAnyMatching : Test
testAnyMatching =
    describe "`any` matching"
        [ test "matches when sample exists" <|
            ((StringParser.use <|
                any (match "f"))
                |> expectToParseNested
                    "f"
                    [ "f" ])
        , test "matches when sample exists several times" <|
            ((StringParser.use <|
                any (match "f"))
                |> expectToParseNested
                    "fff"
                    [ "f", "f", "f" ])
        , test "matches empty list when there were no matches" <|
            ((StringParser.use <|
                any (match "f"))
                |> expectToParseNested
                    ""
                    [ ])
        , test "still matches when sample is not exits" <|
            ((StringParser.use <|
                seqnc [ any (match "f"), match "bar" ])
                |> expectToParseWith
                    "bar"
                    (Matched (Chunk ([Chunks [], Chunk "bar"]))))
        , test "properly advances the position" <|
            ((StringParser.use <|
                getPositionAfter ( any (match "f") ))
                |> expectToParse
                    "ffff"
                    "4")
        ]

testSomeMatching : Test
testSomeMatching =
    describe "`some` matching"
        [ test "matches when sample exists" <|
            ((StringParser.use <|
                some (match "f"))
                |> expectToParseNested
                    "f"
                    [ "f" ])
        , test "matches when sample exists several times" <|
            ((StringParser.use <|
                some (match "f"))
                |> expectToParseNested
                    "fff"
                    [ "f", "f", "f" ])
        , test "keeps the order of occurences" <|
            ((StringParser.use <|
                some
                    (choice
                        [ match "a"
                        , match "b"
                        , match "c"
                        ]
                    ))
                |> expectToParseNested
                    "abc"
                    [ "a", "b", "c" ])
        , test "properly advances position" <|
            ((StringParser.use <|
                getPositionAfter (some (match "f")))
                |> expectToParse
                    "fff"
                    "3")
        , test "not matches when sample is not exits" <|
            ((StringParser.use <|
                seqnc [ some (match "f"), match "bar" ])
                |> expectToFailToParseWith
                    "bar"
                    (Failed (ByExpectation (ExpectedValue "f", GotValue "b")) (0, 0)))
        ]

testAndMatching : Test
testAndMatching =
    describe "`and` matching"
        [ test "matches when sample exists" <|
            ((StringParser.use <|
                seqnc [ Operator.and (match "foo"), match "foobar" ])
                |> expectToParseNested
                    "foobar"
                    [ "", "foobar" ])
        , test "fails when sample not exists" <|
            ((StringParser.use <|
                seqnc [ Operator.and (match "foo"), match "barfoo" ])
                |> expectToFailToParseWith
                    "barfoo"
                    (Failed (ByExpectation (ExpectedValue "foo", GotValue "b")) (0, 0)))
        ]

testNotMatching : Test
testNotMatching =
    describe "`not` matching"
        [ test "fails when sample exists" <|
            ((StringParser.use <|
                seqnc [ Operator.not (match "foo"), match "foobar" ])
                |> expectToFailToParseWith
                    "foobar"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "f")) (0, 0)))
        , test "matches when sample not exists" <|
            ((StringParser.use <|
                seqnc [ Operator.not (match "foo"), match "barfoo" ])
                |> expectToParseNested
                    "barfoo"
                    [ "", "barfoo" ])
        ]

testActionMatching : Test
testActionMatching =
    describe "`action` matching"
        [ test "allows executing user-defined code" <|
            ((StringParser.use <|
                action (match "foo")
                    (\match ctx -> Pass (Chunk "magic")))
                |> expectToParse
                    "foo"
                    "magic")
        , test "provides access to the matched chunk" <|
            ((StringParser.use <|
                action (match "foo")
                    (\match ctx ->
                        case match of
                            Chunk str ->
                                Pass (Chunk (str ++ "magic"))
                            _ -> Pass match))
                |> expectToParse
                    "foo"
                    "foomagic")
        , test "provides access to the position" <|
            ((StringParser.use <|
                action (match "foo")
                    (\match state ->
                        case match of
                            Chunk str ->
                                Pass (Chunk (Basics.toString (state.position)))
                            _ -> Pass match))
                |> expectToParse
                    "foo"
                    "3")
        , test "fails when user-code returned failure even when match was successful by itself" <|
            ((StringParser.use <|
                action (match "foo")
                    (\match ctx -> Fail))
                |> expectToFailToParseWith
                    "foo"
                    (Failed (ByExpectation (ExpectedAnything, GotValue "")) (0, 3)))
        -- TODO: lists etc.
        ]

testPreMatching : Test
testPreMatching =
    describe "`pre` matching"
        [ test "allows executing user-defined code and passes when it returned True" <|
            ((StringParser.use <|
                seqnc
                    [ pre (\_ -> Continue)
                    , (match "foo")
                    ])
                |> expectToParseNested
                    "foo"
                    [ "", "foo" ])
        , test "fails when user-code returned False" <|
            ((StringParser.use <|
                seqnc
                    [ pre (\_ -> Halt)
                    , (match "foo")
                    ])
                |> expectToFailToParseWith
                    "foo"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "f")) (0, 0)))
        , test "provides access to the position" <|
            ((StringParser.use <|
                seqnc
                    [ pre (\state -> if state.position == 0 then Continue else Halt)
                    , (match "foo")
                    ])
                |> expectToParseNested
                    "foo"
                    [ "", "foo" ])
        ]

testNegPreMatching : Test
testNegPreMatching =
    describe "`xpre` matching"
        [ test "allows executing user-defined code and passes when it returned False" <|
            ((StringParser.use <|
                seqnc
                    [ xpre (\_ -> Halt)
                    , (match "foo")
                    ])
                |> expectToParseNested
                    "foo"
                    [ "", "foo" ])
        , test "fails when user-code returned True" <|
            ((StringParser.use <|
                seqnc
                    [ xpre (\_ -> Continue)
                    , (match "foo")
                    ])
                |> expectToFailToParseWith
                    "foo"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "f")) (0, 0)))
        , test "provides access to the position" <|
            ((StringParser.use
                <| seqnc
                    [ xpre (\state -> if state.position /= 0 then Continue else Halt)
                    , (match "foo")
                    ])
                |> expectToParseNested
                    "foo"
                    [ "", "foo" ])
        ]

testLabelMatching : Test
testLabelMatching =
    describe "`label` matching"
        [ test "works transparently for a parser" <|
            ((StringParser.use <|
                label "bar" (match "foo"))
                |> expectToParse
                    "foo"
                    "foo")
        , test "actually stores the value under the given name" <|
            ((StringParser.use <|
                seqnc
                    [ label "xyz" (match "foo")
                    , match "bar"
                    , getLabelValueOrFail "xyz" (match "x")
                    ])
                |> expectToParseNested
                    "foobarx"
                    [ "foo", "bar", "foo" ])
        , test "still fails when match failed" <|
            ((StringParser.use <|
                label "xyz" (match "for"))
                |> expectToFailToParse "foo")
        , test "labels keep the context level when executed in the action call" <|
            ((StringParser.use <|
                seqnc
                    [ label "a" (match "foo")
                    , getLabelValueOrFail "a"
                        ( seqnc
                            [ label "a" (match "bar")
                            , getLabelValueOrFail "a" (match "x")
                            ]
                        )
                    , getLabelValueOrFail "a" (match "z")
                    ])
                |> expectToParseNested
                    "foobarxz"
                    [ "foo", "bar", "foo" ])
        ]

testREMatching : Test
testREMatching =
    describe "`re` matching"
        [ test "properly uses regular expressions to parse text" <|
            ((StringParser.use <|
                re "f?oo")
                |> expectToParse
                    "foo"
                    "foo")
        , test "can parse sequences of symbols" <|
            ((StringParser.use <|
                some (re "[0-9]"))
                |> expectToParseNested
                    "249"
                    [ "2", "4", "9" ])
        , test "properly advances the position" <|
            ((StringParser.use <|
                getPositionAfter (some (re "[0-9]")))
                |> expectToParse
                    "2495"
                    "4")
        , test "fails when regular expression is not matching" <|
            ((StringParser.use <|
                redesc "f?oo" "foo regex")
                |> expectToFailToParseWith
                    "boo"
                    (Failed (ByExpectation (ExpectedRegexMatch "foo regex", GotValue "b")) (0, 0)))
        ]

testDefiningAndCallingRules : Test
testDefiningAndCallingRules =
    describe "defining and calling rules"
        [ test "user should be able to add custom rules" <|
            let
                ruleSpec = match "foo"
                parser = StringParser.withRules
                    [ ( "test", ruleSpec )
                    ] |> Parser.configure
            in
                (\() ->
                    Expect.equal
                        (Just ruleSpec)
                        (parser |> Grammar.getRule "test"))
        , test "user should be able to call rules by name" <|
            (StringParser.withRules
                [ ( "test", match "foo" )
                , ( "start", call "test" )
                ]
                |> Parser.configure
                |> expectToParseAsRule "foo" "foo" "test")
        , test "user should be able to call rules by name, v.2" <|
            (StringParser.withRules
                [ ( "test", match "foo" ) ]
                |> Parser.use (call "test")
                |> Parser.configure
                |> expectToParseAsRule "foo" "foo" "test")
        , test "match should contain a rule name" <|
            (StringParser.withRules
                [ ( "test", match "foo" ) ]
                |> Parser.use (call "test")
                |> Parser.configure
                |> expectToMatchWith
                        "foo"
                        (Chunk "test" (Chunk "foo")))

        , test "failure contains failed rule information" <|
            (StringParser.withRules
                [ ( "test", match "foo" ) ]
                |> Parser.use (call "test")
                |> Parser.configure
                |> expectToFailToParseWith
                    "bar"
                    (Failed (FollowingRule "test"
                        (ByExpectation (ExpectedValue "foo", GotValue "b"))) (0, 0))) -- GotValue "bar"
        ]


testReportingPosition : Test
testReportingPosition =
    describe "test reporting position"
        [ test "no position is passed when match was successful" <|
            ((StringParser.use <|
                match "foo")
                |> Parser.configure
                |> expectToParseWith
                    "foo"
                    (Matched (Chunk "foo")))
        , test "properly reports position of the failure" <|
            ((StringParser.use <|
                seqnc [ match "fo", match "x" ])
                |> Parser.configure
                |> expectToFailToParseAt
                    "foo"
                    (0, 2))
        , test "properly reports position of the failure even for a multiline input" <|
            ((StringParser.use <|
                seqnc [ match "foo", re "[\n]", match "ba", match "x" ])
                |> Parser.configure
                |> expectToFailToParseAt
                    "foo\nbar"
                    (1, 2))
        ]

-- TODO: Test position advances properly for all operators

-- UTILS

nestedFailureOf : List (String, Sample) -> Sample -> Position -> StringParser.ParseResult
nestedFailureOf strings sample position =
    Failed (FollowingNestedOperator
        (List.foldl
            (\(str, sample) failures ->
                failures ++ [ (ByExpectation (ExpectedValue str, sample)) ])
            []
            strings
        , sample)) position

expectToParse : String -> String -> StringParser.Parser -> (() -> Expect.Expectation)
expectToParse input output parser =
    parser |> expectToParseWith
        input
        (Matched (Chunk output))

expectToParseAsRule : String -> String -> String -> StringParser.Parser -> (() -> Expect.Expectation)
expectToParseAsRule input output ruleName parser =
    parser |> expectToParseWith
        input
        (Matched (Match.InRule ruleName (Chunk output)))

expectToParseNested : String -> List String -> StringParser.Parser -> (() -> Expect.Expectation)
expectToParseNested input chunks parser =
    parser |> expectToParseWith
        input
        (Matched (Chunks
                (chunks |> List.map (\chunk -> Chunk chunk))))

getPositionAfter : StringParser.Operator -> StringParser.Operator
getPositionAfter op =
    action op (\_ state -> Pass (Chunk (toString state.position)))

getLabelValueOrFail : String -> StringParser.Operator -> StringParser.Operator
getLabelValueOrFail label op =
    action op
        (\val state ->
            case Dict.get label state.values of
                Just val -> Pass val
                Nothing -> Fail)

failIfLabelHasValue : String -> String -> StringParser.Operator -> StringParser.Operator
failIfLabelHasValue label successVal op =
    action op
        (\val state ->
            case Dict.get label state.values of
                Just _ -> Fail
                Nothing -> Pass (Chunk successVal))
