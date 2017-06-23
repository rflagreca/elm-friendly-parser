port module All exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Test exposing (..)

import ParserTest exposing (suite)
import BasicParserTest exposing (suite)
import CustomParserTest exposing (suite)
import ParserExportTest exposing (suite)


allSuites : Test
allSuites =
    describe "Elm Friendly Parser"
        [ ParserTest.suite
        , BasicParserTest.suite
        , CustomParserTest.suite
        , ParserExportTest.suite
        ]


main : TestProgram
main =
    run emit allSuites


port emit : ( String, Value ) -> Cmd msg
