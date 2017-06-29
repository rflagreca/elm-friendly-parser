module Match exposing
    ( Token(..)
    , Adapter
    )

-- FIXME: there should be an option of UserType contained in this type,
-- this could allow us to get rid of Adapters and stuff
type Token o =
      NoLexem
    | Lexem String
    | Tokens (List (Token o))
    | InRule String (Token o) -- FIXME: RuleName
    | Custom o

type alias Adapter o = (Token o -> o)
