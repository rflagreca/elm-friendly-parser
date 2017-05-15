port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect

import Parser exposing (..)

suite : Test
suite =
    describe "friendly parser"
        [ testStartRule
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


main : TestProgram
main =
    run emit suite


port emit : ( String, Value ) -> Cmd msg
