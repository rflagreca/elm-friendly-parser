module CustomParserTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)

import Samples.ArithmeticsParser as ArithmeticsParser exposing (..)

suite : Test
suite =
    describe "all the custom parsers"
        [ customParserTest
        , arithmeticsParserTest
        ]

-- CUSTOM : THE DEFINITION

type alias MyReturnType = Int

start : Operator MyReturnType -> Parser MyReturnType
start op =
    Parser.start op adapter

adapter : InputType MyReturnType -> MyReturnType
adapter input =
    case input of
        Parser.AValue str -> String.length str
        Parser.AList list -> List.length list
        Parser.ARule name value -> String.length name

-- CUSTOM : THE TEST

customParserTest : Test
customParserTest =
    describe "very custom friendly parser"
        [ test "should use custom adapter to adapt matching values" <|
            \() ->
                let
                    myParser = start <| (match "abc")
                in
                    Expect.equal
                        (Matched 3)
                        (Parser.parse myParser "abc")
        , test "should use custom adapter to adapt matching values, p.2" <|
            \() ->
                let
                    myParser = start <|
                        (seqnc [ match "a", match "b", match "c", match "d" ])
                in
                    Expect.equal
                        (Matched 4)
                        (Parser.parse myParser "abcd")
        , test "still should fail if parsing fails" <|
            \() ->
                let
                    myParser = start <| (match "abc")
                in
                    Expect.equal
                        (Failed (ByExpectation (ExpectedValue "abc", GotValue "a")))
                        (Parser.parse myParser "abz")
        , test "should replace value with the one returned from action code" <|
            \() ->
                let
                    myParser = start <| (action (match "abc") (\_ _ -> Pass 42))
                in
                    Expect.equal
                        (Matched 42)
                        (Parser.parse myParser "abc")
        ]

-- TEST OTHER SAMPLE PARSERS

arithmeticsParserTest : Test
arithmeticsParserTest =
    describe "arithmetics friendly parser"
        [ test "should use custom adapter to adapt matching values" <|
            \() ->
                Expect.equal
                    (Matched 3)
                    (Parser.parse ArithmeticsParser.parser "1 + 2")
        ]
