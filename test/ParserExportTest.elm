module ParserExportTest exposing (suite)

import Test exposing (..)
import Expect

import Parser exposing (..)
import Export exposing (..)

import BasicParser.Parser as BP exposing (..)
import BasicParser.Export as BPExport exposing (..)
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
Matched
    "a"
    "foo", "bar"
    "bar"
                    """
                    (BPExport.parseResult
                        (Parser.Matched
                            (BP.RList (
                                [ BP.RString "a"
                                , BP.RList [ BP.RString "foo", BP.RString "bar" ]
                                , BP.RString "bar"
                                ]
                            ))))
        ]

testExportingFailures : Test
testExportingFailures =
    describe "exporting friendly failures"
        [ ]
