module SimpleParser exposing (..)

import Parser exposing (..)

-- type ReturnType = AString String | AList (List String)

start : Operator -> Parser String ReturnType
start op =
    Parser.withStartRule

adapter : InputType -> InputType
adapter input =
    input
