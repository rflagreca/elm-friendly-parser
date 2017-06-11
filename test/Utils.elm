module Utils exposing (..)

import Parser exposing (..)

import Expect

expectToParse : String -> o -> Parser o -> (() -> Expect.Expectation)
expectToParse input output parser =
    \() ->
        Expect.equal
            (Matched output)
            (Tuple.first (parse parser input))

expectToParseWith : String -> ParseResult o -> Parser o -> (() -> Expect.Expectation)
expectToParseWith input result parser =
    \() ->
        Expect.equal
            result
            (Tuple.first (Parser.parse parser input))

expectToMatchWith : String -> o -> Parser o -> (() -> Expect.Expectation)
expectToMatchWith input value parser =
    parser |> expectToParseWith
        input
        (Matched value)

expectToFailToParse : String -> Parser o -> (() -> Expect.Expectation)
expectToFailToParse input parser =
    \() ->
        let
            ( result, _ ) = parse parser input
        in
            Expect.true
                ("Expected to fail to parse \"" ++ input ++ "\".")
                (isNotParsed result)

expectToFailToParseWith : String -> ParseResult o -> Parser o -> (() -> Expect.Expectation)
expectToFailToParseWith input expectedFailure parser =
    \() ->
        let
            ( result, _ ) = parse parser input
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                actualFailure -> Expect.equal actualFailure expectedFailure

expectToFailToParseAt : String -> Position -> Parser o -> (() -> Expect.Expectation)
expectToFailToParseAt input expectedPosition parser =
     \() ->
        let
            ( result, maybePosition ) = parse parser input
        in
            case result of
                Matched _ -> Expect.fail ("Expected to fail to parse \"" ++ input ++ "\".")
                Failed _ ->
                    case maybePosition of
                        Just actualPosition -> Expect.equal actualPosition expectedPosition
                        Nothing -> Expect.fail ("Expected to receive a position with failure.")

expectToGetResultOfParsing : String -> ( ParseResult o, Maybe Position ) -> Parser o -> (() -> Expect.Expectation)
expectToGetResultOfParsing input result parser =
    \() ->
        Expect.equal result (parse parser input)
