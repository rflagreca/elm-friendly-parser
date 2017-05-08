module Utils exposing (..)

import Tuple exposing (second)
import List exposing (foldl)

-- FIXME: use https://github.com/avh4/elm-transducers or may be there
-- exists an equivalent
--
-- executes the function for every item of `List a`, and while this
-- function returns `Just b`, add `b` instance to a resulting list,
-- but stops execution immediately when function returns `Nothing` for
-- the first time.
--
-- Example:
--
-- ```
-- (iterateMap
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 2, 1, 0, 1, 3, 5, 1, 0, 2, 3 ])
-- -> [ 102, 103, 100, 101, 103 ]
--
-- (iterateMap
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 5, 2, 1, 0, 1, 3, 1, 0, 2, 3 ])
-- -> [ ]
-- ```
iterateMap : (a -> Maybe b) -> List a -> List b
iterateMap f src =
  Tuple.second
    (List.foldl
      (\item acc ->
        case acc of
          ( True, res ) ->
            case (f item) of
              Just val -> ( True, res ++ [ val ] )
              Nothing -> ( False, res )
          ( False, _ ) -> acc)
      ( True, [] )
      src)

-- FIXME: use https://github.com/avh4/elm-transducers or may be there
-- exists an equivalent
--
-- execute the function for every item of `List a`, and when this
-- function returns `Just b` for the first time, return this `b` and
-- stop execution.
--
-- Example:
--
-- ```
-- (iterateOr
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 5, 2, 1, 0, 1, 3, 1, 0, -1, 3 ])
-- -> Just 102
--
-- (iterateOr
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 5, 7, 15, 12, 10, 24, 4, 6, 28 ])
-- -> Nothing
-- ```
iterateOr : (a -> Maybe b) -> List a -> Maybe b
iterateOr f src =
  Tuple.second
    (List.foldl
      (\item acc ->
        case acc of
          ( True, res ) ->
            case (f item) of
              Just val -> ( False, Just val )
              Nothing -> case res of
                Just val -> ( False, res )
                Nothing -> ( True, res )
          ( False, _ ) -> acc)
      ( True, Nothing )
      src)

-- FIXME: use https://github.com/avh4/elm-transducers or may be there
-- exists an equivalent
--
-- execute the function for every item of `List a`, and while this
-- function returns `Just b`, keeps last successful `b` instance to return,
-- but stop execution immediately when function returns `Nothing` for
-- the first time.
--
-- Example:
--
-- ```
-- (iterateWhileAnd
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 2, 1, 0, 1, 3, 5, 1, 0, 2, -1 ])
-- -> Just 103
--
-- (iterateWhileAnd
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 5, 2, 1, 0, 1, 3, 1, 0, 2, 3 ])
-- -> Nothing
-- ```
iterateWhileAnd : (a -> Maybe b) -> List a -> Maybe b
iterateWhileAnd f src =
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
-- execute the function for every item of `List a`, return the last
-- succesful result only when the function returned `Just a` for all
-- the items from the list with no exceptions, else return `Nothing`.
--
-- Example:
--
-- ```
-- (iterateAnd
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 2, 1, 0, 1, 3 ])
-- -> Just 103
--
-- (iterateAnd
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 2, 1, 0, 1, 5 ])
-- -> Nothing
-- ```
iterateAnd : (a -> Maybe b) -> List a -> Maybe b
iterateAnd f src =
  Tuple.second
    (List.foldl
      (\item acc ->
        case acc of
          ( True, res ) ->
            case (f item) of
              Just val -> ( True, Just val )
              Nothing -> ( False, Nothing )
          ( False, _ ) -> ( False, Nothing ))
      ( True, Nothing )
      src)
