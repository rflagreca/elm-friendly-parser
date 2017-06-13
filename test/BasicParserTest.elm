module BasicParserTest exposing (suite)

import Dict

import Test exposing (..)
import Expect

import Parser exposing (..)
import BasicParser.Parser as BasicParser exposing (..)

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

-- (-<) : BasicParser.Operator -> BasicParser
-- (-<) op =
--     BasicParser.start <| op

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            (BasicParser.init
                |> expectToFailToParseWith
                    "foo"
                    (Failed NoStartRule))
        -- FIXME: test allowing to specify custom startRule by name (below, in testDefiningAndCallingRules)
        ]

testBasicMatching : Test
testBasicMatching =
    describe "basic matching"
        [ test "matches simple string" <|
            ((BasicParser.start <|
                (match "abc"))
                |> expectToParse
                    "abc"
                    "abc")
        , test "not matches a string when it is unequeal to the one expected" <|
            ((BasicParser.start <| (match "abc"))
                |> expectToFailToParse "ab")
        , test "fails when input end wasn't reached" <|
            ((BasicParser.start <|
                (match "ab"))
                |> expectToFailToParseWith
                    "abc"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "c"))))
        , test "reports the failed match properly" <|
            ((BasicParser.start <|
                (match "foo"))
                |> expectToFailToParseWith
                    "for"
                    (Failed (ByExpectation (ExpectedValue "foo", GotValue "f")))) -- GotValue "for"
        -- FIXME: test fails when not the whole input matched
        ]

testChoiceMatching : Test
testChoiceMatching =
    describe "`choice` matching"
        [ test "matches correctly" <|
            let
                parser = BasicParser.start <| choice [ match "a", match "b", match "c" ]
            in
                Expect.all
                    [ parser |> expectToParse "a" "a"
                    , parser |> expectToParse "b" "b"
                    , parser |> expectToParse "c" "c"
                    , parser |> expectToFailToParse "d"
                    ]
        , test "fails correctly" <|
            ((BasicParser.start <|
                choice [ match "a", match "b", match "c" ])
                |> expectToFailToParseWith
                    "foo"
                    (nestedFailureOf
                        [ ( "a", (GotValue "f") )
                        , ( "b", (GotValue "f") )
                        , ( "c", (GotValue "f") )
                        ]
                        (GotValue "f")))
        , test "gets first matching result" <|
            ((BasicParser.start <|
                choice [ match "foo", match "f" ])
                |> expectToParse
                    "foo"
                    "foo")
        , test "gets first matching result in a chain" <|
            ((BasicParser.start <|
                choice [ match "a", match "foo", match "f" ])
                |> expectToParse
                    "foo"
                    "foo")
        , test "properly advances position" <|
            ((BasicParser.start <|
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
            ((BasicParser.start <|
                seqnc [ match "f", match "o", match "o" ])
                |> expectToParseNested
                    "foo"
                    [ "f", "o", "o" ])
        , test "fails if one of the operators fails" <|
            ((BasicParser.start <|
                seqnc [ match "f", match "o", match "p" ])
                |> expectToFailToParse "foo")
        , test "fails correctly" <|
            ((BasicParser.start <|
                seqnc [ match "f", match "o", match "p" ])
                |> expectToFailToParseWith
                "foo"
                (Failed (ByExpectation (ExpectedValue "p", GotValue "o" ))))
        ]

testMaybeMatching : Test
testMaybeMatching =
    describe "`maybe` matching"
        [ test "matches when sample exists" <|
            ((BasicParser.start <|
                seqnc [ match "f", match "o", maybe (match "o") ])
                |> expectToParseNested
                    "foo"
                    [ "f", "o", "o" ])
        , test "matches when sample not exists" <|
            ((BasicParser.start <|
                seqnc [ match "f", match "o", maybe (match "o") ])
                |> expectToParseNested
                    "fo"
                    [ "f", "o", "" ])
        , test "matches when sample not exists, p. II" <|
            ((BasicParser.start <|
                seqnc [ match "f", match "o", maybe (match "p"), match "o" ])
                |> expectToParseNested
                    "foo"
                    [ "f", "o", "", "o" ])
        ]

testTextMatching : Test
testTextMatching =
    describe "`text` matching"
        [ test "matches when sample exists" <|
            ((BasicParser.start <|
                text (seqnc [ match "f", match "o", match "o" ]))
                |> expectToParse
                    "foo"
                    "foo")

        , test "still matches when a part of a sample not exists" <|
            ((BasicParser.start <|
                text (seqnc [ match "f", match "o", maybe (match "o") ]))
                |> expectToParse
                    "fo"
                    "fo")
        , test "fails when nested operator is not matching" <|
            ((BasicParser.start <|
                text ( seqnc [ match "f", match "o", match "o" ]))
                |> expectToFailToParseWith
                    "bar"
                    (Failed (ByExpectation (ExpectedValue "f", GotValue "b"))))
        ]

testAnyMatching : Test
testAnyMatching =
    describe "`any` matching"
        [ test "matches when sample exists" <|
            ((BasicParser.start <|
                any (match "f"))
                |> expectToParseNested
                    "f"
                    [ "f" ])
        , test "matches when sample exists several times" <|
            ((BasicParser.start <|
                any (match "f"))
                |> expectToParseNested
                    "fff"
                    [ "f", "f", "f" ])
        , test "matches empty list when there were no matches" <|
            ((BasicParser.start <|
                any (match "f"))
                |> expectToParseNested
                    ""
                    [ ])
        , test "still matches when sample is not exits" <|
            ((BasicParser.start <|
                seqnc [ any (match "f"), match "bar" ])
                |> expectToParseWith
                    "bar"
                    (Matched (RList ([RList [], RString "bar"]))))
        , test "properly advances the position" <|
            ((BasicParser.start <|
                getPositionAfter ( any (match "f") ))
                |> expectToParse
                    "ffff"
                    "4")
        ]

testSomeMatching : Test
testSomeMatching =
    describe "`some` matching"
        [ test "matches when sample exists" <|
            ((BasicParser.start <|
                some (match "f"))
                |> expectToParseNested
                    "f"
                    [ "f" ])
        , test "matches when sample exists several times" <|
            ((BasicParser.start <|
                some (match "f"))
                |> expectToParseNested
                    "fff"
                    [ "f", "f", "f" ])
        , test "keeps the order of occurences" <|
            ((BasicParser.start <|
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
            ((BasicParser.start <|
                getPositionAfter (some (match "f")))
                |> expectToParse
                    "fff"
                    "3")
        , test "not matches when sample is not exits" <|
            ((BasicParser.start <|
                seqnc [ some (match "f"), match "bar" ])
                |> expectToFailToParseWith
                    "bar"
                    (Failed (ByExpectation (ExpectedValue "f", GotValue "b"))))
        ]

testAndMatching : Test
testAndMatching =
    describe "`and` matching"
        [ test "matches when sample exists" <|
            ((BasicParser.start <|
                seqnc [ Parser.and (match "foo"), match "foobar" ])
                |> expectToParseNested
                    "foobar"
                    [ "", "foobar" ])
        , test "fails when sample not exists" <|
            ((BasicParser.start <|
                seqnc [ Parser.and (match "foo"), match "barfoo" ])
                |> expectToFailToParseWith
                    "barfoo"
                    (Failed (ByExpectation (ExpectedValue "foo", GotValue "b" ))))
        ]

testNotMatching : Test
testNotMatching =
    describe "`not` matching"
        [ test "fails when sample exists" <|
            ((BasicParser.start <|
                seqnc [ Parser.not (match "foo"), match "foobar" ])
                |> expectToFailToParseWith
                    "foobar"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "f"))))
        , test "matches when sample not exists" <|
            ((BasicParser.start <|
                seqnc [ Parser.not (match "foo"), match "barfoo" ])
                |> expectToParseNested
                    "barfoo"
                    [ "", "barfoo" ])
        ]

testActionMatching : Test
testActionMatching =
    describe "`action` matching"
        [ test "allows executing user-defined code" <|
            ((BasicParser.start <|
                action (match "foo")
                    (\match ctx -> Pass (BasicParser.RString "magic")))
                |> expectToParse
                    "foo"
                    "magic")
        , test "provides access to the matched chunk" <|
            ((BasicParser.start <|
                action (match "foo")
                    (\match ctx ->
                        case match of
                            BasicParser.RString str ->
                                Pass (BasicParser.RString (str ++ "magic"))
                            _ -> Pass match))
                |> expectToParse
                    "foo"
                    "foomagic")
        , test "provides access to the position" <|
            ((BasicParser.start <|
                action (match "foo")
                    (\match state ->
                        case match of
                            BasicParser.RString str ->
                                Pass (BasicParser.RString (Basics.toString (state.position)))
                            _ -> Pass match))
                |> expectToParse
                    "foo"
                    "3")
        , test "fails when user-code returned failure even when match was successful by itself" <|
            ((BasicParser.start <|
                action (match "foo")
                    (\match ctx -> Fail))
                |> expectToFailToParseWith
                    "foo"
                    (Failed (ByExpectation (ExpectedAnything, GotValue ""))))
        -- TODO: lists etc.
        ]

testPreMatching : Test
testPreMatching =
    describe "`pre` matching"
        [ test "allows executing user-defined code and passes when it returned True" <|
            ((BasicParser.start <|
                seqnc
                    [ pre (\_ -> Continue)
                    , (match "foo")
                    ])
                |> expectToParseNested
                    "foo"
                    [ "", "foo" ])
        , test "fails when user-code returned False" <|
            ((BasicParser.start <|
                seqnc
                    [ pre (\_ -> Halt)
                    , (match "foo")
                    ])
                |> expectToFailToParseWith
                    "foo"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "f"))))
        , test "provides access to the position" <|
            ((BasicParser.start <|
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
            ((BasicParser.start <|
                seqnc
                    [ xpre (\_ -> Halt)
                    , (match "foo")
                    ])
                |> expectToParseNested
                    "foo"
                    [ "", "foo" ])
        , test "fails when user-code returned True" <|
            ((BasicParser.start <|
                seqnc
                    [ xpre (\_ -> Continue)
                    , (match "foo")
                    ])
                |> expectToFailToParseWith
                    "foo"
                    (Failed (ByExpectation (ExpectedEndOfInput, GotValue "f"))))
        , test "provides access to the position" <|
            ((BasicParser.start
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
            ((BasicParser.start <|
                label "bar" (match "foo"))
                |> expectToParse
                    "foo"
                    "foo")
        , test "actually stores the value under the given name" <|
            ((BasicParser.start <|
                seqnc
                    [ label "xyz" (match "foo")
                    , match "bar"
                    , getLabelValueOrFail "xyz" (match "x")
                    ])
                |> expectToParseNested
                    "foobarx"
                    [ "foo", "bar", "foo" ])
        , test "still fails when match failed" <|
            ((BasicParser.start <|
                label "xyz" (match "for"))
                |> expectToFailToParse "foo")
        , test "labels keep the context level when executed in the action call" <|
            ((BasicParser.start <|
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
            ((BasicParser.start <|
                re "f?oo")
                |> expectToParse
                    "foo"
                    "foo")
        , test "can parse sequences of symbols" <|
            ((BasicParser.start <|
                some (re "[0-9]"))
                |> expectToParseNested
                    "249"
                    [ "2", "4", "9" ])
        , test "properly advances the position" <|
            ((BasicParser.start <|
                getPositionAfter (some (re "[0-9]")))
                |> expectToParse
                    "2495"
                    "4")
        , test "fails when regular expression is not matching" <|
            ((BasicParser.start <|
                redesc "f?oo" "foo regex")
                |> expectToFailToParseWith
                    "boo"
                    (Failed (ByExpectation (ExpectedRegexMatch "foo regex", GotValue "b"))))
        ]

testDefiningAndCallingRules : Test
testDefiningAndCallingRules =
    describe "defining and calling rules"
        [ test "user should be able to add custom rules" <|
            let
                ruleSpec = match "foo"
                parser = BasicParser.withRules
                    [ ( "test", ruleSpec )
                    ]
            in
                (\() ->
                    Expect.equal
                        (Just ruleSpec)
                        (parser |> Parser.getRule "test"))
        , test "user should be able to call rules by name" <|
            (BasicParser.withRules
                [ ( "test", match "foo" )
                , ( "start", call "test" )
                ]
                |> expectToParseAsRule "foo" "foo" "test")
        , test "user should be able to call rules by name, v.2" <|
            (BasicParser.withRules
                [ ( "test", match "foo" ) ]
                |> Parser.startWith (call "test")
                |> expectToParseAsRule "foo" "foo" "test")
        , test "match should contain a rule name" <|
            (BasicParser.withRules
                [ ( "test", match "foo" ) ]
                |> Parser.startWith (call "test")
                |> expectToMatchWith
                        "foo"
                        (RRule "test" (RString "foo")))

        , test "failure contains failed rule information" <|
            (BasicParser.withRules
                [ ( "test", match "foo" ) ]
                |> Parser.startWith (call "test")
                |> expectToFailToParseWith
                    "bar"
                    (Failed (FollowingRule "test"
                        (ByExpectation (ExpectedValue "foo", GotValue "b"))))) -- GotValue "bar"

        ]

testReportingPosition : Test
testReportingPosition =
    describe "test reporting position"
        [ test "no position is passed when match was successful" <|
            ((BasicParser.start <|
                match "foo")
                |> expectToGetResultOfParsing
                    "foo"
                    (Matched (BasicParser.RString "foo"), Nothing))
        , test "properly reports position of the failure" <|
            ((BasicParser.start <|
                seqnc [ match "fo", match "x" ])
                |> expectToFailToParseAt
                    "foo"
                    (0, 2))
        , test "properly reports position of the failure even for a multiline input" <|
            ((BasicParser.start <|
                seqnc [ match "foo", re "[\n]", match "ba", match "x" ])
                |> expectToFailToParseAt
                    "foo\nbar"
                    (1, 2))
        ]

-- TODO: Test position advances properly for all operators

-- UTILS

nestedFailureOf : List (String, Sample) -> Sample -> BasicParser.ParseResult
nestedFailureOf strings sample =
    Failed (FollowingNestedOperator
        (List.foldl
            (\(str, sample) failures ->
                failures ++ [ (ByExpectation (ExpectedValue str, sample)) ])
            []
            strings
        , sample))

expectToParse : String -> String -> BasicParser -> (() -> Expect.Expectation)
expectToParse input output parser =
    parser |> expectToParseWith
        input
        (Matched (BasicParser.RString output))

expectToParseAsRule : String -> String -> String -> BasicParser -> (() -> Expect.Expectation)
expectToParseAsRule input output ruleName parser =
    parser |> expectToParseWith
        input
        (Matched (BasicParser.RRule ruleName (BasicParser.RString output)))

expectToParseNested : String -> List String -> BasicParser -> (() -> Expect.Expectation)
expectToParseNested input chunks parser =
    parser |> expectToParseWith
        input
        (Matched (BasicParser.RList
                (chunks |> List.map (\chunk -> RString chunk))))

getPositionAfter : BasicParser.Operator -> BasicParser.Operator
getPositionAfter op =
    action op (\_ state -> Pass (BasicParser.RString (toString state.position)))

getLabelValueOrFail : String -> BasicParser.Operator -> BasicParser.Operator
getLabelValueOrFail label op =
    action op
        (\val state ->
            case Dict.get label state.values of
                Just val -> Pass val
                Nothing -> Fail)

failIfLabelHasValue : String -> String -> BasicParser.Operator -> BasicParser.Operator
failIfLabelHasValue label successVal op =
    action op
        (\val state ->
            case Dict.get label state.values of
                Just _ -> Fail
                Nothing -> Pass (BasicParser.RString successVal))
