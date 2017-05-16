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
        ]

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            expectToFailToParseWith
                "foo"
                NoStartRule
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
    describe "choice matching"
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
                ( Failed ( ExpectedList [ "a", "b", "c" ], GotValue "f" ) )
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
    describe "sequence matching"
        [ test "matches correctly" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| seqnc [ match "f", match "o", match "o" ])
        , test "fails if one of the operators fails" <|
            expectToFailToParse
                "foo"
                (BasicParser.start <| seqnc [ match "f", match "o", match "p" ])
        , test "fails correctly" <|
            expectToFailToParseWith
                "foo"
                ( Failed ( ExpectedValue "p", GotValue "o" ) )
                (BasicParser.start <| seqnc [ match "f", match "o", match "p" ])
        ]

testMaybeMatching : Test
testMaybeMatching =
    describe "maybe matching"
        [ test "matches when sample exists" <|
            expectToParse
                "foo"
                "foo"
                (BasicParser.start <| seqnc [ match "f", match "o", maybe (match "o") ])
        , test "matches when sample not exists" <|
            expectToParse
                "fo"
                "fo"
                (BasicParser.start <| seqnc [ match "f", match "o", maybe (match "o") ])
        , test "fails" <|
            expectToFailToParse
                "foo"
                (BasicParser.start <| seqnc [ match "f", match "o", maybe (match "p") ])
        ]

-- UTILS

expectToParse : String -> String -> BasicParser -> (() -> Expect.Expectation)
expectToParse input output parser =
    \() ->
        Expect.equal
            (Matched (BasicParser.AString output))
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
