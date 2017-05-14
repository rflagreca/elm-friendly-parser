module SimpleParser exposing (..)

import Parser exposing (..)

type ReturnType = AString String | AList (List ReturnType)

start : Operator -> Parser ReturnType
start op =
    Parser.withStartRule op adapter

adapter : InputType ReturnType -> ReturnType
adapter input =
    case input of
        Parser.AString str -> AString str
        Parser.AList list -> AList list
