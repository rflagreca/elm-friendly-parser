module Utils exposing (..)

import Tuple exposing (second)
import List exposing (foldl)

-- executes the function for every item of `List a`, and while this
-- function returns `Just b`, add `b` instance to a a resulting list,
-- but stops execution completely, when function returns `Nothing` first
-- time:
--
-- ```
-- (takeWhileMap
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 2, 1, 0, 1, 3, 5, 1, 0, 2, 3 ])
-- -> [ -8, -9, -10, -9, -7 ]
-- ```
takeWhileMap : (a -> Maybe b) -> List a -> List b
takeWhileMap f src =
  Tuple.second
    (List.foldl
      (\item acc ->
        case acc of
          ( True, res ) ->
            case (f item) of
              Just val -> ( True, res ++ [ val ] )
              Nothing -> ( False, res )
          ( False, res ) -> acc)
      ( True, [] )
      src)
