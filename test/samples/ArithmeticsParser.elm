module Samples.ArithmeticsParser exposing (init)

import Parser exposing (..)

import Regex

type alias ReturnType = Float

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
        (\_ _ -> Fail)
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
        (\_ _ -> Fail)
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
                (\_ _ -> Fail)
            , call "Integer"
            ]
      )
    , ( "Integer"
      , action ( some (re "[0-9]") )
        (\_ _ -> Fail)
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
    42.0
    -- case input of
    --     User.AValue str -> String.length str
    --     User.AList list -> List.length list
    --     User.ARule name value -> String.length name
