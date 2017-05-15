port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect

import Parser exposing (..)
import SimpleParser exposing (..)

suite : Test
suite =
    describe "friendly parser"
        [ testStartRule
        , testBasicMatching
        , testChoiceMatching
        ]

-- TODO: test core Parser, specifying adapters, etc.

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            expectToFailToParseWith
                "foo"
                NoStartRule
                (SimpleParser.withRules Parser.noRules)
        ]

testBasicMatching : Test
testBasicMatching =
    describe "basic matching"
        [ test "matches simple string" <|
            expectToParse
                "abc"
                "abc"
                (SimpleParser.start <| (match "abc"))
        , test "not matches a string when it is unequeal to the one expected" <|
            expectToFailToParse
                "ab"
                (SimpleParser.start <| (match "abc"))
        ]

testChoiceMatching : Test
testChoiceMatching =
    describe "choice matching"
        [ test "matches correctly" <|
            let
                parser = SimpleParser.start <| choice [ match "a", match "b", match "c" ]
            in
                Expect.all
                    [ expectToParse "a" "a" parser
                    , expectToParse "b" "b" parser
                    , expectToParse "c" "c" parser
                    , expectToFailToParse "d" parser
                    ]
        , test "gets first matching result" <|
            expectToParse
                "foo"
                "foo"
                (SimpleParser.start <| choice [ match "foo", match "f" ])
        , test "gets first matching result in a chain" <|
            expectToParse
                "foo"
                "foo"
                (SimpleParser.start <| choice [ match "a", match "foo", match "f" ])
        ]

-- UTILS

expectToParse : String -> String -> SimpleParser -> (() -> Expect.Expectation)
expectToParse input output parser =
    \() ->
        Expect.equal
            (Matched (SimpleParser.AString output))
            (parse parser input)

expectToFailToParse : String -> SimpleParser -> (() -> Expect.Expectation)
expectToFailToParse input parser =
    \() ->
        let
            result = (parse parser input)
        in
            Expect.true
                ("Expected to fail to parse \"" ++ input ++ "\".")
                (isNotParsed result)

expectToFailToParseWith : String -> SimpleParser.ParseResult -> SimpleParser -> (() -> Expect.Expectation)
expectToFailToParseWith input output parser =
    \() ->
        let
            result = (parse parser input)
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                r -> Expect.equal output r


main : TestProgram
main =
    run emit suite


port emit : ( String, Value ) -> Cmd msg
