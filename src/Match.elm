module Match exposing
    ( Token(..)
    , Adapter
    )

type Token o =
      NoLexem
    | Lexem String
    | Tokens (List (Token o))
    | InRule String (Token o) -- FIXME: RuleName
    | My o

type alias Adapter o = (Token o -> o)
