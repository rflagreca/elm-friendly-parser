module Samples.PhoneNumberParser exposing (init)

import Parser exposing (..)
import Operator exposing (..)
import Match

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
       Parser.init adapter
    |> Parser.withRules rules
    |> Parser.setStartRule "phoneNumber"

adapter : Adapter.InputType ReturnType -> ReturnType
adapter input =
    case input of
        Adapter.AValue str -> str
        Adapter.AList list -> String.join "" list
        Adapter.ARule name value -> name ++ ":" ++ value ++ ";"
