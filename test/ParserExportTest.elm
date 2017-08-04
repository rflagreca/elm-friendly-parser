module ParserExportTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)
import Grammar exposing (..)
import Operator exposing (..)
import ParseResult exposing (..)
import Export as Export exposing (..)

import StringParser.Parser as SP exposing (..)
import StringParser.Export as SPExport exposing (..)
import Samples.ArithmeticParser as ArithmeticParser exposing (..)

suite : Test
suite =
    describe "test friendly export"
        [ testExportingParsers
        , testExportingMatches
        , testExportingFailures
        ]

arithmeticPEG : String
arithmeticPEG =
    """
Expression
= head:Term tail:(_ ("+" / "-") _ Term)* {
    return tail.reduce(function(result, element) {
        if (element[1] === "+") { return result + element[3]; }
        if (element[1] === "-") { return result - element[3]; }
    }, head);
    }

Term
= head:Factor tail:(_ ("*" / "/") _ Factor)* {
    return tail.reduce(function(result, element) {
        if (element[1] === "*") { return result * element[3]; }
        if (element[1] === "/") { return result / element[3]; }
    }, head);
    }

Factor
= "(" _ expr:Expression _ ")" { return expr; }
/ Integer

Integer "integer"
= [0-9]+ { return parseInt(text(), 10); }

_ "whitespace"
= [ \\t\\n\\r]*
"""

arithmeticPEGNoCode : String
arithmeticPEGNoCode =
    """
Expression
= head:Term tail:(_ ("+" / "-") _ Term)* { <CODE> }

Term
= head:Factor tail:(_ ("*" / "/") _ Factor)* { <CODE> }

Factor
= "(" _ expr:Expression _ ")" { <CODE> }
/ Integer

Integer "integer"
= [0-9]+ { <CODE> }

_ "whitespace"
= [ \\t\\n\\r]*
"""

testExportingParsers : Test
testExportingParsers =
    describe "exporting friendly parsers"
        [ test "should properly export Arithmetic Parser example as PEG (with no code)" <|
            \() ->
                Expect.equal
                    arithmeticPEGNoCode
                    (Export.parser ArithmeticParser.init)
        ]

testExportingMatches : Test
testExportingMatches =
    describe "exporting friendly matches"
        [ test "should properly export nested match" <|
            \() ->
                Expect.equal
                    """
Matched [ "a", [ "foo", "bar", [ "abc" ], "xyz" ], "bar" ].
"""
{-
                    """
Matched
    "a"
    [ "foo"
    , "bar"
    , [ "abc"
      ]
    , "xyz"
    ]
    "bar"
"""
-}
                    (SPExport.parseResult
                        (Matched
                            (SP.RList (
                                [ SP.RString "a"
                                , SP.RList
                                    [ SP.RString "foo"
                                    , SP.RString "bar"
                                    , SP.RList
                                        [ SP.RString "abc" ]
                                    , SP.RString "xyz"
                                    ]
                                , SP.RString "bar"
                                ]
                            ))))
        ]

testExportingFailures : Test
testExportingFailures =
    describe "exporting friendly failures"
        [ test "should properly export expectation with value and value" <|
            \() ->
                Expect.equal
                    """
Failed at position 20:20 ( line 20, char 20 )

Expected value "a", however got value "b".
"""
                    (SPExport.parseResult
                        (Failed
                            (ByExpectation ( ExpectedValue "a", GotValue "b" ))
                         (20, 20)))

        ]

testExportingResult : Test
testExportingResult =
    describe "exporting friendly result"
        [ test "should properly export the match" <|
            \() ->
                Expect.equal
                    """
Matched [ "foo" ].
"""
                    (SP.start (choice [ match "foo", match "bar" ])
                              |> Parser.parse "foo"
                              |> SPExport.parseResult)
        , test "should properly export the match, p.II" <|
            \() ->
                Expect.equal
                    """
Matched [ "bar" ].
"""
                    (SP.start (choice [ match "foo", match "bar" ])
                              |> Parser.parse "bar"
                              |> SPExport.parseResult)
        ]
