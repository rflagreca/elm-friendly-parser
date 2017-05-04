port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect

import Parser exposing (..)

tests : Test
tests =
    describe "Test"
        [ test "parses a string" <|
            \() ->
                Expect.equal (Match "abc") (parse "abc")
        ]


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
