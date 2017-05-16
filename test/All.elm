port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)

import UtilsTest exposing (suite)
import ParserTest exposing (suite)
import BasicParserTest exposing (suite)


allSuites : Test
allSuites =
    describe "Elm Friendly Parser"
        [ UtilsTest.suite
        , ParserTest.suite
        , BasicParserTest.suite
        ]


main : TestProgram
main =
    run emit allSuites


port emit : ( String, Value ) -> Cmd msg
