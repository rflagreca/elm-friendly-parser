module BasicParser.Export exposing
    ( failureReason
    , parseResult
    )

{-|

@docs failureReason, parseResult

-}

import BasicParser.Parser as BasicParser exposing (..)
import Parser exposing (Position, ParseResult)

repoIssuesUrl : String
repoIssuesUrl = "https://github.com/shamansir/elm-friendly-parser/issues"

returnType : ReturnType -> String
returnType value =
    case value of
        RString str -> "\"" ++ str ++ "\""
        RList list -> "[ " ++ (String.join ", " (List.map returnType list)) ++ " ]"
        RRule name value -> name ++ ": " ++ (returnType value)

-- sample : Parser.Sample -> String
-- sample s =
--     case s of
--         Parser.GotValue str -> "got value \"" ++ str ++ "\""
--         Parser.GotEndOfInput -> "got end of input"

{-| TODO -}
failureReason : Parser.FailureReason ReturnType -> String
failureReason failure =
    case failure of
        Parser.NoStartRule ->
            "Parser has no starting rule defined (use `setStartRule` or `start <|` for that)"
        Parser.SomethingWasNotImplemented ->
            "Seems something was not implemented by the author, feel free to open the issue at " ++ repoIssuesUrl
        Parser.FollowingRule ruleName innerReason ->
            "Following rule `" ++ ruleName ++ "`, which in its turn:" ++ "\n    " ++
                failureReason innerReason
        Parser.FollowingNestedOperator ( nestedFailures, sample ) ->
            let
                sampleStr =
                    case sample of
                        Parser.GotValue str -> "got value \"" ++ str ++ "\""
                        Parser.GotEndOfInput -> "got end of input"
            in
                "Following nested failures of:\n\n" ++
                    (String.join "\n"
                        (List.map (\result -> failureReason failure) nestedFailures))
                ++ ", however " ++ sampleStr
        Parser.ByExpectation ( expectation, sample ) ->
            let
                expectationStr =
                    case expectation of
                        Parser.ExpectedValue str -> "Expected value \"" ++ str ++ "\""
                        Parser.ExpectedAnything -> "Expected anything"
                        Parser.ExpectedRuleDefinition ruleName ->
                            "Expected rule `" ++ ruleName ++ "` to match"
                        Parser.ExpectedRegexMatch rxStr ->
                            "Expected regular expression `" ++ rxStr ++ "` to match"
                        Parser.ExpectedEndOfInput -> "Expected end of input"
                sampleStr =
                    case sample of
                        Parser.GotValue str -> "got value \"" ++ str ++ "\""
                        Parser.GotEndOfInput -> "got end of input"
            in
                expectationStr ++ ", however " ++ sampleStr

{-| TODO -}
parseResult : BasicParser.ParseResult -> Maybe Parser.Position -> String
parseResult result maybePos =
     case result of
        Parser.Matched value -> "\nMatched " ++ (returnType value) ++ ".\n"
        Parser.Failed failure ->
            let
                positionStr =
                    case maybePos of
                        Just ( line, col ) ->
                            "position "
                            ++ toString line ++ ":" ++ toString col ++ " "
                            ++ "( line " ++ toString line ++ ", char " ++ toString col ++ " )"
                        Nothing -> "unknown position"
            in
                "\nFailed at " ++ positionStr ++ "\n\n" ++ (failureReason failure) ++ ".\n"

operator : BasicParser.Operator -> String
operator op =
    "TODO"

rules : BasicParser.Rules -> String
rules rules =
    "TODO"

parser : BasicParser.BasicParser -> String
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
