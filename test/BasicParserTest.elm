module BasicParserTest exposing (suite)

import Dict
import Regex

import Test exposing (..)
import Expect

import Operator exposing (..)
import Parser exposing (..)
import BasicParser.Parser as BasicParser exposing (..)

suite : Test
suite =
    describe "basic friendly parser"
        [ testStartRule
        , testDefiningAndCallingRules
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
        ]

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            expectToFailToParseWith
                "foo"
                (Failed NoStartRule)
                (BasicParser.withRules Parser.noRules)
        -- TODO: allow specifying custom startRule by name
        ]

testDefiningAndCallingRules : Test
testDefiningAndCallingRules =
    describe "defining and calling rules"
        [ test "user should be able to add custom rules" <|
            let
                ruleSpec = match "foo"
                parser = BasicParser.withListedRules
                    [ ( "test", ruleSpec )
                    ]
            in
                (\() ->
                    Expect.equal
                        (Just ruleSpec)
                        (parser |> Parser.getRule "test"))
        , test "user should be able to call rules by name" <|
            let
                parser = BasicParser.withListedRules
                    [ ( "test", match "foo" )
                    , ( "start", call "test" )
                    ]
            in
                expectToParseAsRule "foo" "foo"  "test" parser
        , test "user should be able to call rules by name, v.2" <|
            let
                parser = BasicParser.withListedRules
                    [ ( "test", match "foo" )
                    ]
            in
                expectToParseAsRule "foo" "foo" "test"
                    (parser |> BasicParser.startWith (call "test"))
        , test "match should contain a rule name" <|
            let
                parser = BasicParser.withListedRules
                    [ ( "test", match "foo" )
                    ]
            in
                expectToMatchWith
                    "foo"
                    (RRule "test" (RString "foo"))
                    (parser |> BasicParser.startWith (call "test"))
        , test "failure contains failed rule information" <|
            let
                parser = BasicParser.withListedRules
                    [ ( "test", match "foo" )
                    ]
            in
                expectToFailToParseWith
                    "bar"
                    (Failed (FollowingRule "test"
                        (ByExpectation (ExpectedValue "foo", GotValue "b")))) -- GotValue "bar"
                    (parser |> BasicParser.startWith (call "test"))
        ]

testBasicMatching : Test
testBasicMatching =
    describe "basic matching"
        [ test "matches simple string" <|
            expectToParse
                "abc"
                "abc"
                (BasicParser.start <| (match "abc"))
        , test "not matches a string when it is unequeal to the one expected" <|
            expectToFailToParse
                "ab"
                (BasicParser.start <| (match "abc"))
        , test "reports the failed match properly" <|
            expectToFailToParseWith
                "for"
                ( Failed (ByExpectation (ExpectedValue "foo", GotValue "f") ) ) -- GotValue "for"
                (BasicParser.start <| (match "foo"))
        ]

testChoiceMatching : Test
testChoiceMatching =
    describe "`choice` matching"
        [ test "matches correctly" <|
            let
                parser = BasicParser.start <| choice [ match "a", match "b", match "c" ]
            in
                Expect.all
                    [ expectToParse "a" "a" parser
                    , expectToParse "b" "b" parser
                    , expectToParse "c" "c" parser
                    , expectToFailToParse "d" parser
                    ]
        , test "fails correctly" <|
            expectToFailToParseWith
                "foo"
                (nestedFailureOf
                    [ ( "a", (GotValue "f") )
                    , ( "b", (GotValue "f") )
                    , ( "c", (GotValue "f") )
                    ]
                    (GotValue "f"))
                (BasicParser.start <| choice [ match "a", match "b", match "c" ])
        , test "gets first matching result" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| choice [ match "foo", match "f" ])
        , test "gets first matching result in a chain" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| choice [ match "a", match "foo", match "f" ])
        ]

testSequenceMatching : Test
testSequenceMatching =
    describe "`seqnc` matching"
        [ test "matches correctly" <|
            expectToParseNested
                "foo"
                [ "f", "o", "o" ]
                (BasicParser.start <| seqnc [ match "f", match "o", match "o" ])
        , test "fails if one of the operators fails" <|
            expectToFailToParse
                "foo"
                (BasicParser.start <| seqnc [ match "f", match "o", match "p" ])
        , test "fails correctly" <|
            expectToFailToParseWith
                "foo"
                ( Failed (ByExpectation ( ExpectedValue "p", GotValue "o" ) ) )
                (BasicParser.start <| seqnc [ match "f", match "o", match "p" ])
        ]

testMaybeMatching : Test
testMaybeMatching =
    describe "`maybe` matching"
        [ test "matches when sample exists" <|
            expectToParseNested
                "foo"
                [ "f", "o", "o" ]
                (BasicParser.start <| seqnc [ match "f", match "o", maybe (match "o") ])
        , test "matches when sample not exists" <|
            expectToParseNested
                "fo"
                [ "f", "o", "" ]
                (BasicParser.start <| seqnc [ match "f", match "o", maybe (match "o") ])
        , test "matches when sample not exists, p. II" <|
            expectToParseNested
                "foo"
                [ "f", "o", "" ]
                (BasicParser.start <| seqnc [ match "f", match "o", maybe (match "p") ])
        ]

testTextMatching : Test
testTextMatching =
    describe "`text` matching"
        [ test "matches when sample exists" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| text (seqnc [ match "f", match "o", match "o" ]))
        , test "still matches when a part of a sample not exists" <|
            expectToParse
                "fo"
                "fo"
                (BasicParser.start <| text (seqnc [ match "f", match "o", maybe (match "o") ]))
        , test "fails when nested operator is not matching" <|
            expectToFailToParseWith
                "bar"
                ( Failed (ByExpectation ( ExpectedValue "f", GotValue "b" ) ) )
                (BasicParser.start <| text ( seqnc [ match "f", match "o", match "o" ]))
        ]

testAnyMatching : Test
testAnyMatching =
    describe "`any` matching"
        [ test "matches when sample exists" <|
            expectToParseNested
                "f"
                [ "f" ]
                (BasicParser.start <| any (match "f"))
        , test "matches when sample exists several times" <|
            expectToParseNested
                "fff"
                [ "f", "f", "f" ]
                (BasicParser.start <| any (match "f"))
        , test "still matches when sample is not exits" <|
            expectToParseNested
                "bar"
                [ "", "bar" ]
                (BasicParser.start <| seqnc [ any (match "f"), match "bar" ])
        ]

testSomeMatching : Test
testSomeMatching =
    describe "`some` matching"
        [ test "matches when sample exists" <|
            expectToParseNested
                "f"
                [ "f" ]
                (BasicParser.start <| some (match "f"))
        , test "matches when sample exists several times" <|
            expectToParseNested
                "fff"
                [ "f", "f", "f" ]
                (BasicParser.start <| some (match "f"))
        , test "not matches when sample is not exits" <|
            expectToFailToParseWith
                "bar"
                ( Failed (ByExpectation ( ExpectedValue "f", GotValue "b" ) ) )
                (BasicParser.start <| seqnc [ some (match "f"), match "bar" ])
        ]

testAndMatching : Test
testAndMatching =
    describe "`and` matching"
        [ test "matches when sample exists" <|
            expectToParse
                "foo"
                ""
                (BasicParser.start <| and (match "foo"))
        , test "fails when sample not exists" <|
            expectToFailToParseWith
                "bar"
                ( Failed (ByExpectation ( ExpectedValue "foo", GotValue "b" ) ) )
                (BasicParser.start <| and (match "foo"))
        ]

testNotMatching : Test
testNotMatching =
    describe "`not` matching"
        [ test "fails when sample exists" <|
            expectToFailToParseWith
                "foo"
                ( Failed (ByExpectation ( ExpectedEndOfInput, GotValue "" ) ) )
                (BasicParser.start <| Operator.not (match "foo"))
        , test "matches when sample not exists" <|
            expectToParse
                "bar"
                ""
                (BasicParser.start <| Operator.not (match "foo"))
        ]

testActionMatching : Test
testActionMatching =
    describe "`action` matching"
        [ test "allows executing user-defined code" <|
            expectToParse
                "foo"
                "magic"
                (BasicParser.start <| action (match "foo")
                    (\match ctx -> Pass (BasicParser.RString "magic")))
        , test "provides access to the matched chunk" <|
            expectToParse
                "foo"
                "foomagic"
                (BasicParser.start <| action (match "foo")
                    (\match ctx ->
                        case match of
                            BasicParser.RString str ->
                                Pass (BasicParser.RString (str ++ "magic"))
                            _ -> Pass match))
        , test "provides access to the position" <|
            expectToParse
                "foo"
                "3"
                (BasicParser.start <| action (match "foo")
                    (\match ctx ->
                        case match of
                            BasicParser.RString str ->
                                Pass (BasicParser.RString (Basics.toString (ctx.position)))
                            _ -> Pass match))
        , test "fails when user-code returned failure even when match was successful by itself" <|
            expectToFailToParseWith
                "foo"
                ( Failed ( ByExpectation ( ExpectedAnything, GotValue "" ) ) )
                (BasicParser.start <| action (match "foo")
                    (\match ctx -> Fail))
        -- TODO: lists etc.
        ]

testPreMatching : Test
testPreMatching =
    describe "`pre` matching"
        [ test "allows executing user-defined code and passes when it returned True" <|
            expectToParseNested
                "foo"
                [ "", "foo" ]
                (BasicParser.start <| seqnc
                    [ pre (\_ -> Continue)
                    , (match "foo")
                    ])
        , test "fails when user-code returned False" <|
            expectToFailToParseWith
                "foo"
                ( Failed (ByExpectation ( ExpectedEndOfInput, GotValue "f" ) ) )
                (BasicParser.start <| seqnc
                    [ pre (\_ -> Halt)
                    , (match "foo")
                    ])
        , test "provides access to the position" <|
            expectToParseNested
                "foo"
                [ "", "foo" ]
                (BasicParser.start <| seqnc
                    [ pre (\ctx -> if ctx.position == 0 then Continue else Halt)
                    , (match "foo")
                    ])
        ]

testNegPreMatching : Test
testNegPreMatching =
    describe "`xpre` matching"
        [ test "allows executing user-defined code and passes when it returned False" <|
            expectToParseNested
                "foo"
                [ "", "foo" ]
                (BasicParser.start <| seqnc
                    [ xpre (\_ -> Halt)
                    , (match "foo")
                    ])
        , test "fails when user-code returned True" <|
            expectToFailToParseWith
                "foo"
                ( Failed (ByExpectation ( ExpectedEndOfInput, GotValue "f" ) ) )
                (BasicParser.start <| seqnc
                    [ xpre (\_ -> Continue)
                    , (match "foo")
                    ])
        , test "provides access to the position" <|
            expectToParseNested
                "foo"
                [ "", "foo" ]
                (BasicParser.start <| seqnc
                    [ xpre (\ctx -> if ctx.position /= 0 then Continue else Halt)
                    , (match "foo")
                    ])
        ]

testLabelMatching : Test
testLabelMatching =
    describe "`label` matching"
        [ test "works transparently for a parser" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| label "bar" (match "foo"))
        , test "actually stores the value under the given name" <|
            expectToParseNested
                "foobarx"
                [ "foo", "bar", "foo" ]
                (BasicParser.start <|
                    seqnc
                        [ label "xyz" (match "foo")
                        , match "bar"
                        , action (match "x")
                                 (\val ctx ->
                                    case Dict.get "xyz" ctx.values of
                                        Just val -> Pass val
                                        Nothing -> Fail)
                        ])
        , test "still fails when match failed" <|
            expectToFailToParse
                "foo"
                (BasicParser.start <|
                    label "xyz" (match "for"))
        ]

testREMatching : Test
testREMatching =
    describe "`re` matching"
        [ test "properly uses regular expressions to parse text" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| re (Regex.regex "f?oo") "foo regex")
        , test "fails when regular expression is not matching" <|
            expectToFailToParseWith
                "boo"
                (Failed (ByExpectation (ExpectedRegexMatch "foo regex", GotValue "b")))
                (BasicParser.start <| re (Regex.regex "f?oo") "foo regex")
        ]

-- TODO: Test position advances properly for all operators
-- TODO: Failures should contain either index in the input or (better!) both line/column positions

-- UTILS

nestedFailureOf : List (String, Sample) -> Sample -> BasicParser.ParseResult
nestedFailureOf strings sample =
    Failed (FollowingNestedOperator
        (List.foldl
            (\(str, sample) failures ->
                failures ++ [ Failed (ByExpectation (ExpectedValue str, sample)) ])
            []
            strings
        , sample))

expectToParse : String -> String -> BasicParser -> (() -> Expect.Expectation)
expectToParse input output parser =
    \() ->
        Expect.equal
            (Matched (BasicParser.RString output))
            (BasicParser.parse parser input)

expectToParseAsRule : String -> String -> String -> BasicParser -> (() -> Expect.Expectation)
expectToParseAsRule input output ruleName parser =
    \() ->
        Expect.equal
            (Matched (BasicParser.RRule ruleName (BasicParser.RString output)))
            (BasicParser.parse parser input)

expectToMatchWith : String -> BasicParser.ReturnType -> BasicParser -> (() -> Expect.Expectation)
expectToMatchWith input value parser =
    \() ->
        Expect.equal
            (Matched value)
            (BasicParser.parse parser input)

expectToParseNested : String -> List String -> BasicParser -> (() -> Expect.Expectation)
expectToParseNested input chunks parser =
    \() ->
        Expect.equal
            (Matched (BasicParser.RList
                (chunks |> List.map (\chunk -> RString chunk))))
            (BasicParser.parse parser input)

expectToFailToParse : String -> BasicParser -> (() -> Expect.Expectation)
expectToFailToParse input parser =
    \() ->
        let
            result = (BasicParser.parse parser input)
        in
            Expect.true
                ("Expected to fail to parse \"" ++ input ++ "\".")
                (isNotParsed result)

expectToFailToParseWith : String -> BasicParser.ParseResult -> BasicParser -> (() -> Expect.Expectation)
expectToFailToParseWith input output parser =
    \() ->
        let
            result = (BasicParser.parse parser input)
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                r -> Expect.equal output r
