module Action exposing
    ( ActionResult(..)
    , PrefixActionResult(..)
    , UserCode
    , UserPrefixCode
    )

import State exposing (State)

{-| TODO -}
type ActionResult o = Pass o | PassThrough | Fail -- Return o | PassThrough | Fail
{-| TODO -}
type PrefixActionResult = Continue | Halt -- Continue | Stop (change ChainStep name to End or Exit/ExitWith)

type alias UserCode o = (o -> State o -> (ActionResult o))
type alias UserPrefixCode o = (State o -> PrefixActionResult)
