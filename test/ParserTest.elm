module ParserTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)
import Grammar exposing (..)
import Operator exposing (..)
import Utils exposing (..)
import State exposing (..)
import ParseResult exposing (..)
import Match

zeroPos : Position
zeroPos = (0, 0)

suite : Test
suite =
    describe "core friendly parser"
        [ testStartRule
        , testAdapters
        ]

testStartRule : Test
testStartRule =
    describe "no start rule"
        [ test "should fail to parse anything without \"start\" rule" <|
            (Parser.withRules noRules
                |> expectToFailToParseWith
                    "foo"
                    (Failed NoStartRule zeroPos))
        ]

testAdapters : Test
testAdapters =
    describe "adapters"
        [ test "should parse anything with what adapter returns" <|
            (Parser.use (match "abc")
                |> Parser.adaptWith alwaysTestStringAdapter
                |> expectToParse
                    "abc"
                    "test")
        , test "should parse anything with what adapter returns, p. II" <|
            (Parser.use (match "abc")
                |> Parser.adaptWith (\_ -> "foo")
                |> expectToParse
                    "abc"
                    "foo")
        , test "should provide value of what is parsed" <|
            (Parser.use (match "abc")
                |> Parser.adaptWith
                    (\v -> case v of
                        Match.Lexem s -> (s ++ "d")
                        _ -> "failed")
                |> expectToParse
                    "abc"
                    "abcd")
        ]

alwaysTestStringAdapter : Match.Token String -> String
alwaysTestStringAdapter val =
    "test"

expectToParse : String -> String -> Config o -> (() -> Expect.Expectation)
expectToParse input output config =
    expectToParseWith input (Matched (Match.Lexem output)) config
