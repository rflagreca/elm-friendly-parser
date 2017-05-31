module ParserExportTest exposing (suite)

import Test exposing (..)
import Expect

import Operator exposing (..)
import Parser exposing (..)
import User exposing (..)
import Export exposing (..)

import Samples.ArithmeticsParser as ArithmeticsParser exposing (..)

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
= [ \t\n\r]*
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
= [ \t\n\r]*
    """

suite : Test
suite =
    describe "exporting basic friendly parser"
        [ test "should use custom adapter to adapt matching values" <|
            \() ->
                Expect.equal
                    arithmeticsPEGNoCode
                    (Export.parser ArithmeticsParser.parser)
        ]

