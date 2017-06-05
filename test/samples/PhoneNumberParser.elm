module Samples.PhoneNumberParser exposing (init)

import Parser exposing (..)

type alias ReturnType = String

rules : RulesList ReturnType
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

adapter : InputType ReturnType -> ReturnType
adapter input =
    case input of
        Parser.AValue str -> str
        Parser.AList list -> String.join "" list
        Parser.ARule name value -> name ++ ":" ++ value ++ ";"
