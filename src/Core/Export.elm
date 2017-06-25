module Core.Export exposing
    ( operator
    , rules
    , parser
    )

{-|

@docs operator
    , rules
    , parser

-}

import Dict

import Core.Parser as Parser exposing (..)

rootOperator : Operator o -> String
rootOperator op =
    case op of
        Sequence ops -> String.join " " (List.map operator ops)
        Choice ops -> String.join " / " (List.map rootOperator ops)
        _ -> operator op


{-| TODO -}
operator : Operator o -> String
operator op =
    case op of
        NextChar -> "."
        Match s -> "\"" ++ s ++ "\""
        Maybe_ op -> (operator op) ++ "?"
        Some op -> (operator op) ++ "+"
        Any op -> (operator op) ++ "*"
        Sequence ops -> "(" ++ String.join " " (List.map operator ops) ++ ")"
        Choice ops -> "(" ++ String.join " / " (List.map rootOperator ops) ++ ")"
        Label label op -> label ++ ":" ++ (operator op)
        Action op _ -> (rootOperator op) ++ " { <CODE> }" -- FIXME: implement converting code?
        Call name -> name
        Regex re _ -> toString re
        _ -> "TODO"

rule : RuleName -> Operator o -> String
rule name op =
    name ++ "\n= " ++ (rootOperator op) ++ "\n\n"


{-| TODO -}
rules : Rules o -> String
rules r =
    r |> Dict.foldl
        (\name op str -> str ++ (rule name op))
        "\n"


{-| TODO -}
parser : Parser o -> String
parser p =
    rules p.rules
