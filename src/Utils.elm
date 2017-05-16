module Utils exposing (..)

import Tuple exposing (first)
import List exposing (foldl)

reduce : b -> List a -> (a -> b -> Maybe b) -> b
reduce init src reducer =
  Tuple.first
    (List.foldl
      (\curVal (prevVal, prevContinue) ->
        case prevContinue of
          True -> case (reducer curVal prevVal) of
            Just v -> (v, True)
            Nothing -> (prevVal, False)
          False -> (prevVal, False))
      (init, True)
      src)

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
  reduce [] src
    (\cur allValues ->
      case (f cur) of
        Just nextValue -> Just ( allValues ++ [ nextValue ] )
        Nothing -> Nothing
    )

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
  reduce Nothing src
    (\cur hasValue ->
      case hasValue of
        Just v -> Just hasValue
        Nothing -> case (f cur) of
          Just v -> Just ( Just v )
          Nothing -> Just Nothing
    )

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
    (reduce ( True, Nothing ) src
      (\cur ( hasValue, _ ) ->
        case hasValue of
          True -> case (f cur) of
            Just v -> Just ( True, Just v )
            Nothing -> Just ( False, Nothing )
          False -> Just ( False, Nothing )
      )
    )

-- execute the function for every item of `List a`, return all the
-- collected results only when the function returned `Just a` for all
-- the items from the list with no exceptions, else return `Nothing`.
--
-- Example:
--
-- ```
-- (iterateMapAnd
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 2, 1, 0, 1, 3 ])
-- -> Just [ 102, 101, 100, 101, 103 ]
--
-- (iterateMapAnd
--   (\n -> if (n <= 3) then Just (n + 100) else Nothing)
--   [ 2, 1, 0, 1, 5 ])
-- -> Nothing
-- ```
iterateMapAnd : (a -> Maybe b) -> List a -> Maybe (List b)
iterateMapAnd f src =
  Tuple.second
    (reduce ( True, Nothing ) src
      (\cur ( hasValue, maybeAllValues ) ->
        case hasValue of
          True -> case (f cur) of
            Just nextValue ->
              case maybeAllValues of
                Just allValues -> Just ( True, Just ( allValues ++ [ nextValue ] ) )
                Nothing -> Just ( True, Just [ nextValue ] )
            Nothing -> Just ( False, Nothing )
          False -> Just ( False, Nothing )
      )
    )

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
    (reduce ( True, Nothing ) src
      (\cur ( hasValue, maybeLastValue ) ->
        case hasValue of
          True -> case (f cur) of
            Just v -> Just ( True, Just v )
            Nothing -> case maybeLastValue of
              Just lastValue -> Just ( False, Just lastValue )
              Nothing -> Just ( False, Nothing )
          False -> Just ( False, Nothing )
      )
    )
