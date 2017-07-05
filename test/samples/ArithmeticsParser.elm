module Samples.ArithmeticsParser exposing (..)

import Parser exposing (..)
import Operator exposing (..)
import Match exposing (..)
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

reduceAdditionAndSubtraction : Token ReturnType -> Float -> Float
reduceAdditionAndSubtraction triplet sum =
    case triplet of
        (Tokens (_::(My (Operator op))::_::(My (Number v))::_) ) ->
            case op of
                Add -> sum + v
                Subtract -> sum - v
                _ -> -1
        _ -> -1

reduceMultiplicationAndDivision : Token ReturnType -> Float -> Float
reduceMultiplicationAndDivision triplet sum =
    case triplet of
        (Tokens (_::(My (Operator op))::_::(My (Number v))::_) ) ->
            case op of
                Multiply -> sum * v
                Divide -> sum / v
                _ -> -1
        _ -> -1

{- The action of the "integer" rule. -}
integerAction : Token ReturnType -> State ReturnType -> ActionResult ReturnType
integerAction source _ =
    case source of
        Tokens maybeDigits ->
            case digitsToFloat maybeDigits of
                Just value -> Pass (Number value)
                Nothing -> Fail
        _ -> Fail

{- The action of the "Expression" rule. -}
expressionAction : Token ReturnType -> State ReturnType -> ActionResult ReturnType
expressionAction _ state =
    let
        maybeHead = (Dict.get "head" state.values)
        maybeTail = (Dict.get "tail" state.values)
        reducer = reduceAdditionAndSubtraction
    in
        case ( maybeHead, maybeTail ) of
            ( Just head, Just tail ) ->
                case ( head, tail ) of
                    ( My (Number headNum), Tokens tailList ) ->
                        Pass (Number (List.foldl reducer headNum tailList))
                    _ -> Fail
            _ -> Fail

{- The action of the "Term" rule. -}
termAction : Token ReturnType -> State ReturnType -> ActionResult ReturnType
termAction _ state =
    let
        maybeHead = (Dict.get "head" state.values)
        maybeTail = (Dict.get "tail" state.values)
        reducer = reduceMultiplicationAndDivision
    in
        case ( maybeHead, maybeTail ) of
            ( Just head, Just tail ) ->
                case ( head, tail ) of
                    ( My (Number headNum), Tokens tailList ) ->
                        Pass (Number (List.foldl reducer headNum tailList))
                    _ -> Fail
            _ -> Fail

{- The action used inside the "Factor" rule. -}
extractExpressionAction : Token ReturnType -> State ReturnType -> ActionResult ReturnType
extractExpressionAction _ state =
    case Debug.log "expr" (Dict.get "expr" state.values) of
        Just val ->
            case val of
                (My (Number n)) -> Pass (Number n)
                _ -> Fail
        Nothing -> Fail

{- Convert a list of potential digits to a float. -}
digitsToFloat : List (Token ReturnType) -> Maybe Float
digitsToFloat maybeDigits =
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
        case List.foldl collapse (Just "") maybeDigits of
            Just digitsString -> String.toFloat digitsString |> Result.toMaybe
            Nothing -> Nothing
