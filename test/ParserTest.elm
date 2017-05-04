port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect


tests : Test
tests =
    describe "Test"
        [ test "truly working" <|
            \() ->
                Expect.equal True True
        , test "falsey working" <|
            \() ->
                Expect.equal False False
        ]


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg
