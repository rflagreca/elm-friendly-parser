module ParseResult exposing
    ( ParseResult(..)
    , Expectation(..)
    , Sample(..)
    , FailureReason(..)
    )

import State exposing (Position)
import Match exposing (Token)

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
