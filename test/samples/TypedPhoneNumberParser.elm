module Samples.TypedPhoneNumberParser exposing (..)

import Parser exposing (..)
import Operator exposing (..)
import Action exposing (..)
import Match
import ParseResult exposing (..)

type PhoneNumberPart =
    Unknown
  | Prefix String Int
  | Operator Int
  | Local (Int, Int, Int)
  | PhoneNumber
    { prefix: (String, Int)
    , operator: Int
    , local: (Int, Int, Int)
    }

type alias ReturnType = PhoneNumberPart

rules : Rules ReturnType
rules =
    [ ( "phoneNumber"
      , action
        (seqnc
            [ maybe (call "prefix")
            , maybe (call "operator")
            , (call "local")
            ]
        )
        (\val _ -> extractPhoneNumber val)
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
      , action
        (seqnc
            [ choice [ match "(", match "[" ]
            , some (re "[0-9]")
            , choice [ match "]", match ")" ]
            ]
        )
        (\val _ -> extractOperator val)
      )
    , ( "local"
      , action
        ( seqnc
            [ some (re "[0-9]")
            , match "-"
            , some (re "[0-9]")
            , match "-"
            , some (re "[0-9]")
            ]
        )
        (\val _ -> extractLocal val)
      )
    ]

init : Parser ReturnType
init =
       Parser.init
    |> Parser.withRules rules
    |> Parser.setStartRule "phoneNumber"

adapter : Match.Token PhoneNumberPart -> ReturnType
adapter result =
    case result of
        Match.My v -> v
        _ -> Unknown

isAString : Match.Token PhoneNumberPart -> Bool
isAString test =
    case test of
        Match.Lexem _ -> True
        _ -> False

digitsToInt : List (Match.Token ReturnType) -> Maybe Int
digitsToInt probablyDigits =
    let
        collapse =
            (\val prev ->
                case prev of
                    Just prevDigits ->
                        case val of
                            Match.Lexem a ->
                                Just (prevDigits ++ a)
                            _ -> Nothing
                    Nothing -> Nothing)
    in
        case List.foldl collapse (Just "") probablyDigits of
            Just digitsString -> String.toInt digitsString |> Result.toMaybe
            Nothing -> Nothing

extractPrefix : Match.Token PhoneNumberPart -> ActionResult PhoneNumberPart
extractPrefix source =
  case source of
    Match.Tokens vals ->
      if List.length vals == 2 then
        case vals of
          (Match.Lexem symbol)::(Match.Tokens maybeDigits)::_ ->
            case digitsToInt maybeDigits of
                Just value -> Pass (Prefix symbol value)
                Nothing -> Fail
          _ -> Fail
      else Fail
    _ -> Fail

extractOperator : Match.Token PhoneNumberPart -> ActionResult PhoneNumberPart
extractOperator source =
    case source of
        Match.Tokens vals ->
            if List.length vals == 3 then
                case vals of
                    _::(Match.Tokens maybeDigits)::_ ->
                        case digitsToInt maybeDigits of
                            Just value -> Pass (Operator value)
                            Nothing -> Fail
                    _ -> Fail
            else Fail
        _ -> Fail

extractLocal : Match.Token PhoneNumberPart -> ActionResult PhoneNumberPart
extractLocal source =
    case source of
        Match.Tokens vals ->
            if List.length vals == 5 then
                case vals of
                    (Match.Tokens maybeDigits1)::_::(Match.Tokens maybeDigits2)::_::(Match.Tokens maybeDigits3)::_ ->
                        case ( digitsToInt maybeDigits1
                             , digitsToInt maybeDigits2
                             , digitsToInt maybeDigits3
                             ) of
                            ( Just digits1
                            , Just digits2
                            , Just digits3
                            ) -> Pass (Local (digits1, digits2, digits3))
                            _ -> Fail
                    _ -> Fail
            else Fail
        _ -> Fail

extractPhoneNumber : Match.Token PhoneNumberPart -> ActionResult PhoneNumberPart
extractPhoneNumber source =
    case source of
        Match.Tokens vals ->
            if List.length vals == 3 then
                case vals of
                    Match.My (Prefix symbol number)
                  ::Match.My (Operator operatorNumber)
                  ::Match.My (Local (local1, local2, local3))
                  ::_ ->
                        Pass
                            (PhoneNumber
                                { prefix = ( symbol, number )
                                , operator = operatorNumber
                                , local = ( local1, local2, local3 )
                                }
                            )
                    _ -> Fail
            else Fail
        _ -> Fail

