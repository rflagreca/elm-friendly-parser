module ParseResult exposing
    ( ParseResult(..)
    , MyParseResult(..)
    , Expectation(..)
    , Sample(..)
    , FailureReason(..)
    , toMyResult
    )

import State exposing (Position)
import Match exposing (Token, Adapter)

type Expectation =
      ExpectedValue String -- FIXME: Rename: ExpectedLexem, MatchType?
    | ExpectedAnything
    | ExpectedRuleDefinition String -- FIXME: RuleName
    | ExpectedRegexMatch String
    --| ExpectedStartRule
    | ExpectedEndOfInput

type Sample =
      GotValue String
    | GotEndOfInput

type FailureReason o = -- FIXME: Remove `o`
      ByExpectation ( Expectation, Sample )
    | FollowingRule String (FailureReason o) -- FIXME: RuleName
    | FollowingNestedOperator ( List (FailureReason o), Sample )
    | NoStartRule
    | SomethingWasNotImplemented

type ParseResult o =
      Matched (Token o)
    | Failed (FailureReason o) Position

type MyParseResult o =
      MatchedMy o
    | FailedMy (FailureReason o) Position

toMyResult : Adapter o -> ParseResult o -> MyParseResult o
toMyResult adapter result =
    case result of
        Matched token -> MatchedMy (adapter token)
        Failed failure position -> FailedMy failure position
