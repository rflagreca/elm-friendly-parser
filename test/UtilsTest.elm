module UtilsTest exposing (suite)

import Test exposing (..)
import Expect

import Utils exposing (..)

suite : Test
suite =
    describe "utils"
        [ testReduce
        , testIterateMap
        ]

testReduce : Test
testReduce =
    describe "reduce"
        [ test "should iterate through a list while function returns something" <|
            (\() ->
                Expect.equal
                    3
                    (reduce -1 [ 0, 1, 2, 3 ]
                        (\n _ -> Just n))
            )
        , test "should stop when function returns `Nothing`" <|
            (\() ->
                Expect.equal
                    1
                    (reduce -1 [ 0, 1, 2, 3 ]
                        (\n _ -> if n < 2 then Just n else Nothing))
            )
        , test "should return initial value when first element returned nothing" <|
            (\() ->
                Expect.equal
                    -1
                    (reduce -1 [ 0, 1, 2, 3 ] (\_ _ -> Nothing) )
            )
        , test "should be able to join items" <|
            (\() ->
                Expect.equal
                    5
                    (reduce -1 [ 0, 1, 2, 3 ]
                        (\n prevN -> Just (n + prevN)))
            )
        , test "should provide access to previous items" <|
            (\() ->
                Expect.equal
                    [ 1, 2, 3, 4 ]
                    (reduce  [] [ 0, 1, 2, 3 ]
                        (\n prev -> Just (prev ++ [ n + 1 ])))
            )
        , test "should work with strings" <|
            (\() ->
                Expect.equal
                    "3/2/1/0/"
                    (reduce "" [ 0, 1, 2, 3 ]
                        (\n prev -> Just (toString n ++ "/" ++ prev)))
            )
        ]

testIterateMap : Test
testIterateMap =
    describe "iterateMap"
        [ test "should convert all elements while `f` returns something" <|
            (\() ->
                Expect.equal
                    [ 0, 1, 2, 3 ]
                    (iterateMap (\n -> Just n) [ 0, 1, 2, 3 ])
            )
        , test "should apply what mapping function returns" <|
            (\() ->
                Expect.equal
                    [ 10, 11, 12, 13 ]
                    (iterateMap (\n -> Just (n + 10)) [ 0, 1, 2, 3 ])
            )
        , test "should stop when mapping function returns `Nothing`" <|
            (\() ->
                Expect.equal
                    [ 0, 1 ]
                    (iterateMap
                        (\n -> if (n < 2) then Just n else Nothing )
                        [ 0, 1, 2, 3 ])
            )
        , test "should return empty array when function returned `Nothing` for the first element" <|
            (\() ->
                Expect.equal
                    [ ]
                    (iterateMap
                        (\n -> if (n < 0) then Just n else Nothing )
                        [ 0, -1, -2, -3 ])
            )
        , test "should apply mapping function despite being stopped by `Nothing`" <|
            (\() ->
                Expect.equal
                    [ -10, -9, -8 ]
                    (iterateMap
                        (\n -> if (n < 3) then Just (n - 10) else Nothing )
                        [ 0, 1, 2, 3 ])
            )
        ]
