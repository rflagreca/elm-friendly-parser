module Samples.PhoneNumberParser exposing (init)

import Parser exposing (..)
import Grammar exposing (..)
import Operator exposing (..)
import Match
import ParseResult exposing (..)
type alias ReturnType = String

rules : Rules ReturnType
rules =
    [ ( "phoneNumber"
      , seqnc
        [ maybe (call "prefix")
        , maybe (call "operator")
        , (call "local")
        ]
      )
    , ( "prefix"
      , seqnc
        [ match "+"
        , some (re "[0-9]")
        ]
      )
    , ( "operator"
      , seqnc
        [ choice [ match "(", match "[" ]
        , some (re "[0-9]")
        , choice [ match "]", match ")" ]
        ]
      )
    , ( "local"
      , seqnc
        [ some (re "[0-9]")
        , match "-"
        , some (re "[0-9]")
        , match "-"
        , some (re "[0-9]")
        ]
      )
    ]

init : Parser ReturnType
init =
       Parser.init
    |> Parser.use rules
    |> Parser.setStartRule "phoneNumber"

parse : String -> MyParseResult ReturnType
parse input =
  Parser.init
    |> Parser.parse input
    |> toMyResult adapter

adapter : Match.Token ReturnType -> ReturnType
adapter input =
    case input of
        Match.NoLexem -> ""
        Match.Lexem str -> str
        Match.Tokens list -> String.join "" (List.map adapter list)
        Match.InRule name value -> name ++ ":" ++ (adapter value) ++ ";"
        Match.My my -> my
