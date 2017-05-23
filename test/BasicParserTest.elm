module BasicParserTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)
import BasicParser exposing (..)

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
        ]

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            expectToFailToParseWith
                "foo"
                (Failed NoStartRule)
                (BasicParser.withRules Parser.noRules)
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
                (BasicParser.start <| Parser.not (match "foo"))
        , test "matches when sample not exists" <|
            expectToParse
                "bar"
                ""
                (BasicParser.start <| Parser.not (match "foo"))
        ]

testActionMatching : Test
testActionMatching =
    describe "`action` matching"
        [ test "allows executing user-defined code" <|
            expectToParse
                "foo"
                "magic"
                (BasicParser.start <| action (match "foo")
                    (\match ctx -> Matched (BasicParser.RString "magic")))
        , test "provides access to the matched chunk" <|
            expectToParse
                "foo"
                "foomagic"
                (BasicParser.start <| action (match "foo")
                    (\match ctx ->
                        case match of
                            BasicParser.RString str ->
                                Matched (BasicParser.RString (str ++ "magic"))
                            BasicParser.RList list -> Matched (BasicParser.RList list)))
        , test "provides access to the position" <|
            expectToParse
                "foo"
                "3"
                (BasicParser.start <| action (match "foo")
                    (\match ctx ->
                        case match of
                            BasicParser.RString str ->
                                Matched (BasicParser.RString (Basics.toString (ctx.position)))
                            BasicParser.RList list -> Matched (BasicParser.RList list)))
        , test "fails when user-code returned failure even if match is successful by itself" <|
            expectToFailToParseWith
                "foo"
                (Failed SomethingWasNotImplemented)
                (BasicParser.start <| action (match "foo")
                    (\match ctx -> (Failed SomethingWasNotImplemented)))
        -- TODO
        ]

-- TODO: Test position advances properly for all operators

-- UTILS

nestedFailureOf : List (String, Sample) -> Sample -> BasicParser.ParseResult
nestedFailureOf strings sample =
    Failed (FollowingNestedRule
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
            (parse parser input)

expectToParseNested : String -> List String -> BasicParser -> (() -> Expect.Expectation)
expectToParseNested input chunks parser =
    \() ->
        Expect.equal
            (Matched (BasicParser.RList
                (chunks |> List.map (\chunk -> RString chunk))))
            (parse parser input)

expectToFailToParse : String -> BasicParser -> (() -> Expect.Expectation)
expectToFailToParse input parser =
    \() ->
        let
            result = (parse parser input)
        in
            Expect.true
                ("Expected to fail to parse \"" ++ input ++ "\".")
                (isNotParsed result)

expectToFailToParseWith : String -> BasicParser.ParseResult -> BasicParser -> (() -> Expect.Expectation)
expectToFailToParseWith input output parser =
    \() ->
        let
            result = (parse parser input)
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                r -> Expect.equal output r
