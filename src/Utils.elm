module Utils exposing (..)

import Tuple exposing (second)
import List exposing (foldl)

-- FIXME: use https://github.com/avh4/elm-transducers or may be there
-- exists an equivalent
--
-- executes the function for every item of `List a`, and while this
-- function returns `Just b`, add `b` instance to a resulting list,
-- but stops execution immediately when function returns `Nothing` first
-- time.
--
-- Example:
--
-- ```
-- (takeWhileMap
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 2, 1, 0, 1, 3, 5, 1, 0, 2, 3 ])
-- -> [ -8, -9, -10, -9, -7 ]
--
-- (takeWhileMap
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 5, 2, 1, 0, 1, 3, 1, 0, 2, 3 ])
-- -> [ ]
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

-- FIXME: use https://github.com/avh4/elm-transducers or may be there
-- exists an equivalent
--
-- execute the function for every item of `List a`, and while this
-- function returns `Just b`, keeps last successful `b` instance to return,
-- but stop execution immediately when function returns `Nothing` first
-- time.
--
-- Example:
--
-- ```
-- (takeTillLastSuccess
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 2, 1, 0, 1, 3, 5, 1, 0, 2, 3 ])
-- -> Just -7
--
-- (takeTillLastSuccess
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 5, 2, 1, 0, 1, 3, 1, 0, 2, 3 ])
-- -> Nothing
-- ```
takeTillLastSuccess : (a -> Maybe b) -> List a -> Maybe b
takeTillLastSuccess f src =
  Tuple.second
    (List.foldl
      (\item acc ->
        case acc of
          ( True, res ) ->
            case (f item) of
              Just val -> ( True, Just val )
              Nothing -> ( False, res )
          ( False, _ ) -> acc)
      ( True, Nothing )
      src)


-- FIXME: use https://github.com/avh4/elm-transducers or may be there
-- exists an equivalent
--
-- execute the function for every item of `List a`, and when this
-- function returns `Just b` first time, return this `b` and stop execution.
--
-- Example:
--
-- ```
-- (takeTillFirstSuccess
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 5, 2, 1, 0, 1, 3, 1, 0, 2, 3 ])
-- -> Just -8
--
-- (takeTillFirstSuccess
--   (\n -> if (n <= 3) then Just (n - 10) else Nothing)
--   [ 5, 7, 15, 12, 10, 24, 4, 6, 28 ])
-- -> Nothing
-- ```
takeTillFirstSuccess : (a -> Maybe b) -> List a -> Maybe b
takeTillFirstSuccess f src =
  Tuple.second
    (List.foldl
      (\item acc ->
        case acc of
          ( True, res ) ->
            case (f item) of
              Just val -> ( True, Just val )
              Nothing -> case res of
                Just val -> ( False, res )
                Nothing -> ( True, res )
          ( False, _ ) -> acc)
      ( True, Nothing )
      src)
