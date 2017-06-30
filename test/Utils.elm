module Utils exposing (..)

import Parser exposing (..)
import ParseResult exposing (..)
import Match exposing (..)
import State exposing (Position)

import Expect

-- expectToParse : String -> String -> Parser String -> (() -> Expect.Expectation)
-- expectToParse input output parser =
--     parser |> expectToParseWith input (Matched (Custom output))

expectToParseWith : String -> ParseResult o -> Parser o -> (() -> Expect.Expectation)
expectToParseWith input result parser =
    \() ->
        Expect.equal
            result
            (parser |> parse input)

expectToMatchWith : String -> o -> Parser o -> (() -> Expect.Expectation)
expectToMatchWith input value parser =
    parser |> expectToParseWith
        input
        (Matched (Custom value))

expectToFailToParse : String -> Parser o -> (() -> Expect.Expectation)
expectToFailToParse input parser =
    \() ->
        Expect.true
            ("Expected to fail to parse \"" ++ input ++ "\".")
            (isNotParsed (parser |> parse input))

expectToFailToParseWith : String -> ParseResult o -> Parser o -> (() -> Expect.Expectation)
expectToFailToParseWith input expectedFailure parser =
    \() ->
        let
            result = parser |> parse input
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                actualFailure -> Expect.equal actualFailure expectedFailure

expectToFailToParseAt : String -> Position -> Parser o -> (() -> Expect.Expectation)
expectToFailToParseAt input expectedPosition parser =
     \() ->
        case parser |> parse input of
            Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
            Failed _ position ->
                Expect.equal position expectedPosition

-- expectToGetResultOfParsing : String -> ( ParseResult o, Maybe Position ) -> Parser o -> (() -> Expect.Expectation)
-- expectToGetResultOfParsing input result parser =
--     \() ->
--         Expect.equal result (parser |> parse input)


isNotParsed : ParseResult o -> Bool
isNotParsed result =
    case result of
        Matched _ -> False
        Failed _ _ -> True


-- isParsedAs : String -> ParseResult o -> Bool
-- isParsedAs subject result =
--     case result of
--         Matched s -> (toString s == subject)
--         Failed _ _ -> False
