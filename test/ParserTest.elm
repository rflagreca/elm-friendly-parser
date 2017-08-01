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
                |> Parser.configure
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
                |> Parser.configure
                |> expectToParse
                    "abc"
                    "test")
        , test "should parse anything with what adapter returns, p. II" <|
            (Parser.use (match "abc")
                |> Parser.adaptWith (\_ -> "foo")
                |> Parser.configure
                |> expectToParse
                    "abc"
                    "foo")
        , test "should provide value of what is parsed" <|
            (Parser.use (match "abc")
                |> Parser.adaptWith
                    (\v -> case v of
                        Match.Lexem s -> (s ++ "d")
                        _ -> "failed")
                |> Parser.configure
                |> expectToParse
                    "abc"
                    "abcd")
        ]

alwaysTestStringAdapter : Match.Token String -> String
alwaysTestStringAdapter val =
    "test"

expectToParse : String -> String -> Parser o -> (() -> Expect.Expectation)
expectToParse input output parser =
    expectToParseWith input (Matched (Match.Lexem output)) parser
