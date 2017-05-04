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
                (start <| (match "abc"))
                (Matched "abc")
        ]

testChoiceMatching : Test
testChoiceMatching =
    describe "choice matching"
        [ test "matches correctly" <|
            let
                choiceParser = start <| choice [ match "a", match "b", match "c" ]
            in
                Expect.all
                    [ expectToParse "a" choiceParser (Matched "a")
                    , expectToParse "b" choiceParser (Matched "b")
                    ]
        ]

expectToParse : String -> Parser -> ParseResult -> (() -> Expect.Expectation)
expectToParse input parser result =
    \() ->
        Expect.equal
            result
            (parse parser input)

main : TestProgram
main =
    run emit suite


port emit : ( String, Value ) -> Cmd msg
