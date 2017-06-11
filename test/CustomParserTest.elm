module CustomParserTest exposing (suite)

import Test exposing (..)

import Parser exposing (..)

import Samples.ArithmeticsParser as ArithmeticsParser exposing (..)
import Samples.PhoneNumberParser as PhoneNumberParser exposing (..)
import Samples.TypedPhoneNumberParser as TypedPhoneNumberParser exposing (..)

import Utils exposing (..)

suite : Test
suite =
    describe "all the custom parsers"
        [ customParserTest
        , arithmeticsParserTest
        , phoneNumberParserTest
        , typedPhoneNumberParserTest
        ]

-- CUSTOM : THE DEFINITION

type alias MyReturnType = Int

start : Operator MyReturnType -> Parser MyReturnType
start op =
    Parser.start op adapter

adapter : InputType MyReturnType -> MyReturnType
adapter input =
    case input of
        Parser.AValue str -> String.length str
        Parser.AList list -> List.length list
        Parser.ARule name value -> String.length name

-- CUSTOM : THE TEST

customParserTest : Test
customParserTest =
    describe "very custom friendly parser"
        [ test "should use custom adapter to adapt matching values" <|
            expectToMatchWith
                "abc"
                3
                (start <| (match "abc"))
        , test "should use custom adapter to adapt matching values, p.2" <|
            expectToMatchWith
                "abcd"
                4
                (start <|
                    (seqnc [ match "a", match "b", match "c", match "d" ]))
        , test "still should fail if parsing fails" <|
            expectToFailToParseWith
                "abz"
                (Failed (ByExpectation (ExpectedValue "abc", GotValue "a")))
                (start <| (match "abc"))
        , test "should replace value with the one returned from action code" <|
            expectToMatchWith
                "abc"
                42
                (start <| (action (match "abc") (\_ _ -> Pass 42)))
        ]

-- TEST OTHER SAMPLE PARSERS

arithmeticsParserTest : Test
arithmeticsParserTest =
    describe "arithmetics friendly parser"
        [ test "should parse the expression" <|
            (ArithmeticsParser.init |>
                expectToMatchWith
                    "2 * (3 + 4)"
                    (ArithmeticsParser.ANumber 14))
        ]

phoneNumberParserTest : Test
phoneNumberParserTest =
    describe "phone number friendly parser"
        [ test "should parse the phone number" <|
            (PhoneNumberParser.init |>
                expectToMatchWith
                    "+35[057]776-22-13"
                    "prefix:+35;operator:[057];local:776-22-13;")
        ]

typedPhoneNumberParserTest : Test
typedPhoneNumberParserTest =
    describe "phone number friendly parser"
        [ test "should parse the phone number" <|
            (TypedPhoneNumberParser.init |>
                expectToMatchWith
                    "+35[057]776-22-13"
                    (TypedPhoneNumberParser.PhoneNumber
                        { prefix = ("+", 35)
                        , operator = 057
                        , local = (776, 22, 13)
                        }
                    )
            )
        ]
