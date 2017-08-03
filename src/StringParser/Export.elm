module StringParser.Export exposing
    ( failureReason
    , parseResult
    )

{-|

@docs failureReason, parseResult

-}

import StringParser.Parser as StringParser exposing (..)
import ParseResult exposing (..)
import State exposing (Position)
import Match

repoIssuesUrl : String
repoIssuesUrl = "https://github.com/shamansir/elm-friendly-parser/issues"

returnType : StringParser.ReturnType -> String
returnType value =
    case value of
        StringParser.Chunk str -> "\"" ++ str ++ "\""
        StringParser.Chunks list -> "[ " ++ (List.map returnType list |> String.join ", ") ++ " ]"
        StringParser.InRule name v -> name ++ ":" ++ (returnType v)

-- sample : Parser.Sample -> String
-- sample s =
--     case s of
--         Parser.GotValue str -> "got value \"" ++ str ++ "\""
--         Parser.GotEndOfInput -> "got end of input"

{-| TODO -}
failureReason : FailureReason StringParser.ReturnType -> String
failureReason failure =
    case failure of
        NoStartRule ->
            "Parser has no starting rule defined (use `setStartRule` or `start <|` for that)"
        SomethingWasNotImplemented ->
            "Seems something was not implemented by the author, feel free to open the issue at " ++ repoIssuesUrl
        FollowingRule ruleName innerReason ->
            "Following rule `" ++ ruleName ++ "`, which in its turn:" ++ "\n    " ++
                failureReason innerReason
        FollowingNestedOperator ( nestedFailures, sample ) ->
            let
                sampleStr =
                    case sample of
                        GotValue str -> "got value \"" ++ str ++ "\""
                        GotEndOfInput -> "got end of input"
            in
                "Following nested failures of:\n\n" ++
                    (String.join "\n"
                        (List.map (\result -> failureReason failure) nestedFailures))
                ++ ", however " ++ sampleStr
        ByExpectation ( expectation, sample ) ->
            let
                expectationStr =
                    case expectation of
                        ExpectedValue str -> "Expected value \"" ++ str ++ "\""
                        ExpectedAnything -> "Expected anything"
                        ExpectedRuleDefinition ruleName ->
                            "Expected rule `" ++ ruleName ++ "` to match"
                        ExpectedRegexMatch rxStr ->
                            "Expected regular expression `" ++ rxStr ++ "` to match"
                        ExpectedEndOfInput -> "Expected end of input"
                sampleStr =
                    case sample of
                        GotValue str -> "got value \"" ++ str ++ "\""
                        GotEndOfInput -> "got end of input"
            in
                expectationStr ++ ", however " ++ sampleStr

token : StringParser.Token -> String
token aToken =
    case aToken of
        Match.NoLexem -> "Nothing"
        Match.Lexem str -> "\"" ++ str ++ "\""
        Match.Tokens tokens ->  "[ " ++ (String.join ", " (List.map token tokens)) ++ " ]"
        Match.InRule ruleName innerToken -> ruleName ++ ": " ++ (token innerToken)
        Match.My str -> returnType str

{-| TODO -}
parseResult : StringParser.ParseResult -> String
parseResult result =
     case result of
        Matched value ->
            "\nMatched " ++ (token value) ++ ".\n"
        Failed failure position ->
            let
                positionStr =
                    case position of
                        ( line, col ) ->
                            "position "
                            ++ toString line ++ ":" ++ toString col ++ " "
                            ++ "( line " ++ toString line ++ ", char " ++ toString col ++ " )"
            in
                "\nFailed at " ++ positionStr ++ "\n\n" ++ (failureReason failure) ++ ".\n"

operator : StringParser.Operator -> String
operator op =
    "TODO"

rules : StringParser.Rules -> String
rules rules =
    "TODO"

parser : StringParser.Parser -> String
parser parser =
    "TODO"

-- fromString : String -> Parser o
-- fromString src =
--     "TODO"

-- encode : Parser o -> Json
-- encode parser =
--     "TODO"

-- decode : Json -> Parser o
-- decode json =
--     "TODO"
