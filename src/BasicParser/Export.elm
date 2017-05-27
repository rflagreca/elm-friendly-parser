module BasicParser.Export exposing (..)

import BasicParser.Parser as BasicParser exposing (..)
import Operator exposing (ParseResult)

returnType : ReturnType -> String
returnType value =
    case value of
        RString str -> str
        RList list -> String.join "," (List.map returnType list)
        RRule name value -> name ++ ": " ++ (returnType value)

failureReason : Operator.FailureReason ReturnType -> String
failureReason failure =
    "TODO"

parseResult : BasicParser.ParseResult -> String
parseResult result =
     case result of
        Operator.Matched value -> "Matched " ++ (returnType value)
        Operator.Failed failure -> "Failed " ++ (failureReason failure)

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
