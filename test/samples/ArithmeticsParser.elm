module Samples.ArithmeticsParser exposing (..)

import Parser exposing (..)
import Operator exposing (..)
import Match
import State exposing (..)
import Action exposing (..)

import Dict

type OperatorKind = Add | Subtract | Multiply | Divide

{- The possible ReturnType for this parser, just extends the basic types with `ANumber` -}
type ReturnType = Number Float | Operator OperatorKind

{- The actual grammar for this parser. See `arithmetics.peg` in this directory for the source PEG Grammar -}
rules : Rules ReturnType
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
                    , call "whitespace"
                    , match ")"
                    ]
                )
                extractExpressionAction
            , call "Integer"
            ]
      )
    , ( "Integer"
      , action ( any (re "[0-9]") )
        integerAction
      )
    , ( "whitespace"
      , any (re "[ \t\n\r]")
      )
    ]

{- init and set start rule to "Expression" -}
init : Parser ReturnType
init =
       Parser.init
    |> Parser.withRules rules
    |> Parser.setStartRule "Expression"

reduceAdditionAndSubtraction : ReturnType -> Float -> Float
reduceAdditionAndSubtraction triplet sum =
    case triplet of
        (AList (_::Operator op::_::ANumber v::_) ) ->
            case op of
                "+" -> sum + v
                "-" -> sum - v
                _ -> -1
        _ -> -1

reduceMultiplicationAndDivision : ReturnType -> Float -> Float
reduceMultiplicationAndDivision triplet sum =
    case triplet of
        (AList (_::AString op::_::ANumber v::_) ) ->
            case op of
                "*" -> sum * v
                "/" -> sum / v
                _ -> -1
        _ -> -1

{- The action of the "integer" rule. -}
integerAction : ReturnType -> State ReturnType -> ActionResult ReturnType
integerAction source _ =
    case source of
        AList maybeDigits ->
            case digitsToFloat maybeDigits of
                Just value -> Pass (ANumber value)
                Nothing -> Fail
        _ -> Fail

{- The action of the "Expression" rule. -}
expressionAction : ReturnType -> State ReturnType -> ActionResult ReturnType
expressionAction _ state =
    let
        maybeHead = (Dict.get "head" state.values)
        maybeTail = (Dict.get "tail" state.values)
        reducer = reduceAdditionAndSubtraction
    in
        case ( maybeHead, maybeTail ) of
            ( Just head, Just tail ) ->
                case ( head, tail ) of
                    ( ANumber headNum, AList tailList ) ->
                        Pass (ANumber (List.foldl reducer headNum tailList))
                    _ -> Fail
            _ -> Fail

{- The action of the "Term" rule. -}
termAction : ReturnType -> State ReturnType -> ActionResult ReturnType
termAction _ state =
    let
        maybeHead = (Dict.get "head" state.values)
        maybeTail = (Dict.get "tail" state.values)
        reducer = reduceMultiplicationAndDivision
    in
        case ( maybeHead, maybeTail ) of
            ( Just head, Just tail ) ->
                case ( head, tail ) of
                    ( ANumber headNum, AList tailList ) ->
                        Pass (ANumber (List.foldl reducer headNum tailList))
                    _ -> Fail
            _ -> Fail

{- The action used inside the "Factor" rule. -}
extractExpressionAction : ReturnType -> State ReturnType -> ActionResult ReturnType
extractExpressionAction _ state =
    case Dict.get "expr" state.values of
        Just val -> Pass val
        Nothing -> Fail

{- Convert a list of potential digits to a float. -}
digitsToFloat : List ReturnType -> Maybe Float
digitsToFloat probablyDigits =
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
            Just digitsString -> String.toFloat digitsString |> Result.toMaybe
            Nothing -> Nothing
