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
            expectToFailToParseWith
                "foo"
                (Failed NoStartRule)
                (Parser.init alwaysTestStringAdapter)
        ]

testAdapters : Test
testAdapters =
    describe "adapters"
        [ test "should parse anything with what adapter returns" <|
            expectToParse
                "abc"
                "test"
                (Parser.start (match "abc") alwaysTestStringAdapter)
        , test "should parse anything with what adapter returns, p. II" <|
            expectToParse
                "abc"
                "foo"
                (Parser.start (match "abc") (\_ -> "foo"))
        , test "should provide value of what is parsed" <|
            expectToParse
                "abc"
                "abcd"
                (Parser.start
                    (match "abc")
                    (\v -> case v of
                        Parser.AValue s -> (s ++ "d")
                        _ -> "failed"))
        ]

alwaysTestStringAdapter : InputType String -> String
alwaysTestStringAdapter val =
    "test"
