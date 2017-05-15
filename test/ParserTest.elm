module ParserTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)

suite : Test
suite =
    describe "core friendly parser"
        [ testStartRule
        , testAdapters
        ]

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            expectToFailToParseWith
                "foo"
                NoStartRule
                (Parser.withRules Parser.noRules alwaysTestStringAdapter)
        ]

testAdapters : Test
testAdapters =
    describe "adapters"
        [ test "should parse anything with what adapter returns" <|
            expectToParse
                "abc"
                "test"
                (Parser.start (match "abc") alwaysTestStringAdapter)
        , test "should parse anything with what adapter returns, p. II" <|
            expectToParse
                "abc"
                "foo"
                (Parser.start (match "abc") (\_ -> "foo"))
        , test "should provide value of what is parsed" <|
            expectToParse
                "abc"
                "abcd"
                (Parser.start
                    (match "abc")
                    (\v -> case v of
                        Parser.AString s -> (s ++ "d")
                        _ -> "failed"))
        ]

alwaysTestStringAdapter : InputType String -> String
alwaysTestStringAdapter val =
    "test"

-- UTILS

expectToParse : String -> o -> Parser o -> (() -> Expect.Expectation)
expectToParse input output parser =
    \() ->
        Expect.equal
            (Matched output)
            (parse parser input)

expectToFailToParse : String -> Parser o -> (() -> Expect.Expectation)
expectToFailToParse input parser =
    \() ->
        let
            result = (parse parser input)
        in
            Expect.true
                ("Expected to fail to parse \"" ++ input ++ "\".")
                (isNotParsed result)

expectToFailToParseWith : String -> ParseResult o -> Parser o -> (() -> Expect.Expectation)
expectToFailToParseWith input output parser =
    \() ->
        let
            result = (parse parser input)
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                r -> Expect.equal output r
