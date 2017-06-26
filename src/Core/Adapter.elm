module Core.Adapter exposing
    ( InputType(..)
    , Adapter
    )

type InputType o =
      AValue String
    | AList (List o)
    | ARule String o -- FIXME: RuleName

type alias Adapter o = (InputType o -> o)
