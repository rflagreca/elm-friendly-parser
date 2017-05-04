port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect

import Parser exposing (..)

suite : Test
suite =
    describe "friendly parser"
        [ testBasicMatching
        , testChoiceMatching
        ]

testBasicMatching : Test
testBasicMatching =
    describe "basic matching"
        [ test "matches simple string" <|
            expectToParse
                "abc"
                "abc"
                (start <| (match "abc"))
        , test "not matches a string when it is unequeal to the one expected" <|
            expectNotToParse
                "ab"
                (start <| (match "abc"))
        ]

testChoiceMatching : Test
testChoiceMatching =
    describe "choice matching"
        [ test "matches correctly" <|
            let
                parser = start <| choice [ match "a", match "b", match "c" ]
            in
                Expect.all
                    [ expectToParse "a" "a" parser
                    , expectToParse "b" "b" parser
                    , expectToParse "c" "c" parser
                    , expectNotToParse "d" parser
                    ]
        ]

-- UTILS

expectToParse : String -> String -> Parser -> (() -> Expect.Expectation)
expectToParse input output parser =
    \() ->
        Expect.equal
            (Matched output)
            (parse parser input)

expectNotToParse : String -> Parser -> (() -> Expect.Expectation)
expectNotToParse input parser =
    \() ->
        let
            result = (parse parser input)
        in
            Expect.true
                ("Expected \"" ++ input ++ "\" not to parse.")
                (isNotParsed result)


main : TestProgram
main =
    run emit suite


port emit : ( String, Value ) -> Cmd msg
