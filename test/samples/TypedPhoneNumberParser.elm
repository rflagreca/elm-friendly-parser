module Samples.TypedPhoneNumberParser exposing (..)

import Parser exposing (..)

type PhoneNumberPart =
    AString String
  | AList (List PhoneNumberPart)
  | Dash
  | Prefix String Int
  | Operator Int
  | Local (Int, Int, Int)
  | PhoneNumber
    { prefix: (String, Int)
    , operator: Int
    , local: (Int, Int, Int)
    }

type ReturnType = PhoneNumberPart

rules : RulesList PhoneNumberPart
rules =
    [ ( "phoneNumber"
      , seqnc
        [ maybe (call "prefix")
        , maybe (call "operator")
        , (call "local")
        ]
      )
    , ( "prefix"
      , action
        ( seqnc
            [ match "+"
            , some (re "[0-9]")
            ]
        )
        (\val _ -> extractPrefix val)
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
        , action (match "-") (\_ _ -> Pass Dash)
        , some (re "[0-9]")
        , action (match "-") (\_ _ -> Pass Dash)
        , some (re "[0-9]")
        ]
      )
    ]

init : Parser PhoneNumberPart
init =
       Parser.init adapter
    |> Parser.withRules rules
    |> Parser.setStartRule "phoneNumber"

adapter : InputType PhoneNumberPart -> PhoneNumberPart
adapter input =
    case input of
        Parser.AValue str -> AString str
        Parser.AList list -> AList list
        Parser.ARule name value -> value

extractPrefix : PhoneNumberPart -> ActionResult PhoneNumberPart
extractPrefix source =
  case source of
    AList vals ->
      if List.length vals == 2 then
        case vals of
          (AString symbol)::(AString number)::_ ->
            Pass (Prefix symbol
                 (String.toInt number |> Result.withDefault 0))
          _ -> Fail
      else Fail
    _ -> Fail

extractDash : PhoneNumberPart -> State PhoneNumberPart -> ActionResult PhoneNumberPart
extractDash _ _ =
    Pass Dash

-- extractLocal : PhoneNumberPart -> PhoneNumberPart
