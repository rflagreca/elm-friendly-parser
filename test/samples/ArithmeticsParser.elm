module Samples.ArithmeticsParser exposing (..)

import Parser exposing (..)

import Regex
import Dict

type ReturnType =
     AString String
   | AList (List ReturnType)
   | ANumber Int
   -- | Operator String

-- Expression
--   = head:Term tail:(_ ("+" / "-") _ Term)* {
--       return tail.reduce(function(result, element) {
--         if (element[1] === "+") { return result + element[3]; }
--         if (element[1] === "-") { return result - element[3]; }
--       }, head);
--     }

-- Term
--   = head:Factor tail:(_ ("*" / "/") _ Factor)* {
--       return tail.reduce(function(result, element) {
--         if (element[1] === "*") { return result * element[3]; }
--         if (element[1] === "/") { return result / element[3]; }
--       }, head);
--     }

-- Factor
--   = "(" _ expr:Expression _ ")" { return expr; }
--   / Integer

-- Integer "integer"
--   = [0-9]+ { return parseInt(text(), 10); }

-- _ "whitespace"
--   = [ \t\n\r]*

rules : RulesList ReturnType
rules =
    [ ( "Expression"
      , action
            (seqnc
                [ label "head" (call "Term")
                , label "tail"
                    (any
                        (seqnc
                            [ call "whitespace"
                            , choice [ match "+", match "-" ]
                            , call "whitespace"
                            , call "Term"
                            ]
                        )
                    )
                ]
            )
        expressionAction
      )
    , ( "Term"
      , action
            ( seqnc
                [ label "head" (call "Factor")
                , label "tail"
                    (any
                        (seqnc
                            [ call "whitespace"
                            , choice [ match "*", match "/" ]
                            , call "whitespace"
                            , call "Factor"
                            ]
                        )
                    )
                ]
            )
        termAction
      )
    , ( "Factor"
      , choice
            [ action
                ( seqnc
                    [ match "("
                    , call "whitespace"
                    , label "expr" (call "Expression")
                    ]
                )
                extractExpressionAction
            , call "Integer"
            ]
      )
    , ( "Integer"
      , action ( some (re "[0-9]") )
        integerAction
      )
    , ( "whitespace"
      , any (re "[ \t\n\r]")
      )
    ]

init : Parser ReturnType
init =
       Parser.init adapter
    |> Parser.withRules rules
    |> Parser.setStartRule "Expression"

adapter : InputType ReturnType -> ReturnType
adapter input =
    case input of
        Parser.AValue str -> AString str
        Parser.AList list -> AList list
        Parser.ARule name value -> value

digitsToInt : List ReturnType -> Maybe Int
digitsToInt probablyDigits =
    let
        collapse =
            (\val prev ->
                case prev of
                    Just prevDigits ->
                        case val of
                            AString a ->
                                Just (prevDigits ++ a)
                            _ -> Nothing
                    Nothing -> Nothing)
    in
        case List.foldl collapse (Just "") probablyDigits of
            Just digitsString -> String.toInt digitsString |> Result.toMaybe
            Nothing -> Nothing

integerAction : ReturnType -> State ReturnType -> ActionResult ReturnType
integerAction source _ =
    case source of
        AList maybeDigits ->
            case digitsToInt maybeDigits of
                Just value -> Pass (ANumber value)
                Nothing -> Fail
        _ -> Fail

expressionAction : ReturnType -> State ReturnType -> ActionResult ReturnType
expressionAction _ state =
    let
        maybeHead = (Dict.get "head" state.values)
        maybeTail = (Dict.get "tail" state.values)
        reducer = (\result element -> Pass result)
    in
        case ( maybeHead, maybeTail ) of
            ( Just head, Just tail ) -> Pass head
            -- ( Just head, Just tail ) -> List.foldl reducer (Pass head) tail
            _ -> Fail

termAction : ReturnType -> State ReturnType -> ActionResult ReturnType
termAction _ state =
    let
        maybeHead = (Dict.get "head" state.values)
        maybeTail = (Dict.get "tail" state.values)
    in
        case ( maybeHead, maybeTail ) of
            ( Just head, Just tail ) -> Pass head
            _ -> Fail

extractExpressionAction : ReturnType -> State ReturnType -> ActionResult ReturnType
extractExpressionAction _ state =
    case (Dict.get "expr" state.values) of
        Just val -> Pass val
        Nothing -> Fail
