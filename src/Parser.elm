module Parser exposing (..)

import Dict exposing (..)

type Operator = Operator (String -> Operator)

type ParseResult = Match String | MatchError

type alias ParserOptions = {
    rules: Dict String Operator
}

parse : String -> ParseResult
parse input =
    Match input


