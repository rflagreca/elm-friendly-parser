module Adapter exposing
    ( InputType(..)
    , Adapter
    )

-- FIXME: there should be an option of UserType contained in this type,
-- this could allow us to get rid of Adapters and stuff
type InputType o =
      AValue String
    | AList (List o)
    | ARule String o -- FIXME: RuleName

type alias Adapter o = (InputType o -> o)
