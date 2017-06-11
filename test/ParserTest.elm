module ParserTest exposing (suite)

import Test exposing (..)

import Parser exposing (..)
import Utils exposing (..)

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
            ((Parser.init alwaysTestStringAdapter)
                |> expectToFailToParseWith
                    "foo"
                    (Failed NoStartRule))
        ]

testAdapters : Test
testAdapters =
    describe "adapters"
        [ test "should parse anything with what adapter returns" <|
            ((Parser.start (match "abc") alwaysTestStringAdapter)
                |> expectToParse
                    "abc"
                    "test")
        , test "should parse anything with what adapter returns, p. II" <|
            ((Parser.start (match "abc") (\_ -> "foo"))
                |> expectToParse
                    "abc"
                    "foo")
        , test "should provide value of what is parsed" <|
            ((Parser.start
                (match "abc")
                (\v -> case v of
                    Parser.AValue s -> (s ++ "d")
                    _ -> "failed"))
                |> expectToParse
                    "abc"
                    "abcd")
        ]

alwaysTestStringAdapter : InputType String -> String
alwaysTestStringAdapter val =
    "test"
