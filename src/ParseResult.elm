module ParseResult exposing
    ( ParseResult(..)
    , Expectation(..)
    , Sample(..)
    , FailureReason(..)
    , Position
    )

type alias Position = ( Int, Int )

type Expectation =
      ExpectedValue String -- FIXME: InputType?
    | ExpectedAnything
    | ExpectedRuleDefinition String -- FIXME: RuleName
    | ExpectedRegexMatch String
    --| ExpectedStartRule
    | ExpectedEndOfInput

type Sample =
      GotValue String
    | GotEndOfInput

type FailureReason o =
      ByExpectation ( Expectation, Sample )
    | FollowingRule String (FailureReason o)  -- FIXME: RuleName
    | FollowingNestedOperator ( List (FailureReason o), Sample )
    | NoStartRule
    | SomethingWasNotImplemented

type ParseResult o =
      Matched o
    | Failed (FailureReason o) Position
