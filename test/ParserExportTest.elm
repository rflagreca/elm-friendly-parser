module ParserExportTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)
import Grammar exposing (..)
import ParseResult exposing (..)
import Export as Export exposing (..)

import StringParser.Parser as BP exposing (..)
import StringParser.Export as BPExport exposing (..)
import Samples.ArithmeticsParser as ArithmeticsParser exposing (..)

suite : Test
suite =
    describe "test friendly export"
        [ testExportingParsers
        , testExportingMatches
        , testExportingFailures
        ]

arithmeticsPEG : String
arithmeticsPEG =
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

arithmeticsPEGNoCode : String
arithmeticsPEGNoCode =
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
        [ test "should properly export Arithmetics Parser example as PEG (with no code)" <|
            \() ->
                Expect.equal
                    arithmeticsPEGNoCode
                    (Export.parser ArithmeticsParser.init)
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
                    (BPExport.parseResult
                        (Matched
                            (BP.RList (
                                [ BP.RString "a"
                                , BP.RList
                                    [ BP.RString "foo"
                                    , BP.RString "bar"
                                    , BP.RList
                                        [ BP.RString "abc" ]
                                    , BP.RString "xyz"
                                    ]
                                , BP.RString "bar"
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
                    (BPExport.parseResult
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
                    (BP.start (choice [ match "foo", match "bar" ])
                              |> Parser.parse "foo"
                              |> BPExport.parseResult)
        , test "should properly export the match, p.II" <|
            \() ->
                Expect.equal
                    """
Matched [ "bar" ].
"""
                    (BP.start (choice [ match "foo", match "bar" ])
                              |> Parser.parse "bar"
                              |> BPExport.parseResult)
        ]
