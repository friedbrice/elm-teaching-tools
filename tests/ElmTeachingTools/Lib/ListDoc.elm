module ElmTeachingTools.Lib.ListDoc exposing (..)

import Dict
import ElmTeachingTools.Lib.List exposing (..)
import Expect
import Test


suite : Test.Test
suite =
    Test.describe "ElmTeachingTools.Lib.List"
        [ Test.describe "iterate"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (iterate
                            (\x ->
                                if x == 3 then
                                    Nothing

                                else
                                    Just (x + 1)
                            )
                            0
                        )
                        [ 1, 2, 3 ]
            ]
        , Test.describe "unfold"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (unfold String.uncons "slowly")
                        [ 's', 'l', 'o', 'w', 'l', 'y' ]
            , Test.test "Can simulate `iterate`." <|
                \_ ->
                    Expect.equal
                        (unfold
                            (\x ->
                                if x == 3 then
                                    Nothing

                                else
                                    Just ( x + 1, x + 1 )
                            )
                            0
                        )
                        [ 1, 2, 3 ]
            , Test.test "More general than `iterate`." <|
                \_ ->
                    Expect.equal
                        (unfold
                            (\x ->
                                if x == 3 then
                                    Nothing

                                else
                                    Just ( x ^ 2, x + 1 )
                            )
                            0
                        )
                        [ 0, 1, 4 ]
            ]
        , Test.describe "fromMaybe"
            [ Test.test "Empty maybe yields empty list." <|
                \_ ->
                    Expect.equal
                        (fromMaybe Nothing)
                        []
            , Test.test "Populated maybe yields singleton list." <|
                \_ ->
                    Expect.equal
                        (fromMaybe (Just 5))
                        [ 5 ]
            ]
        , Test.describe "dropWhile"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (dropWhile (\x -> x < 5) [ 1, 2, 3, 4, 5, 3 ])
                        [ 5, 3 ]
            ]
        , Test.describe "takeWhile"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (takeWhile (\x -> x < 5) [ 1, 2, 3, 4, 5, 3 ])
                        [ 1, 2, 3, 4 ]
            ]
        , Test.describe "filterMaybes"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (filterMaybes [ Just 1, Just 2, Nothing, Just 4 ])
                        [ 1, 2, 4 ]
            ]
        , Test.describe "zip"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (zip [ 1, 2, 3 ] [ "a", "b", "c", "d" ])
                        [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ) ]
            ]
        , Test.describe "cross"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (cross [ 1, 2, 3 ] [ 'a', 'b' ])
                        [ ( 1, 'a' ), ( 1, 'b' ), ( 2, 'a' ), ( 2, 'b' ), ( 3, 'a' ), ( 3, 'b' ) ]
            ]
        , Test.describe "join"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (join (\x y -> modBy 2 (x + y) == 0) [ 1, 2, 3 ] [ 4, 5, 6 ])
                        [ ( 1, 5 ), ( 2, 4 ), ( 2, 6 ), ( 3, 5 ) ]
            ]
        , Test.describe "inc"
            [ Test.test "Advance to next element of the list." <|
                \_ ->
                    Expect.equal
                        (inc [ "do", "re", "mi" ] "re")
                        (Just "mi")
            , Test.test "Fails when the element does not appear in the list." <|
                \_ ->
                    Expect.equal
                        (inc [ "do", "mi", "so" ] "re")
                        Nothing
            , Test.test "Fails when the element appears only at the end of the list." <|
                \_ ->
                    Expect.equal
                        (inc [ "do", "re", "mi" ] "mi")
                        Nothing
            , Test.test "Fails when the list is empty." <|
                \_ ->
                    Expect.equal
                        (inc [] "mi")
                        Nothing
            ]
        , Test.describe "dec"
            [ Test.test "Go back to the previous element of the list." <|
                \_ ->
                    Expect.equal
                        (dec [ "do", "re", "mi" ] "re")
                        (Just "do")
            , Test.test "Fails when the element does not appear in the list." <|
                \_ ->
                    Expect.equal
                        (dec [ "do", "mi", "so" ] "re")
                        Nothing
            , Test.test "Fails when the element appears at the beginning of the list." <|
                \_ ->
                    Expect.equal
                        (dec [ "do", "re", "mi" ] "do")
                        Nothing
            , Test.test "Fails when the list is empty." <|
                \_ ->
                    Expect.equal
                        (dec [] "do")
                        Nothing
            ]
        , Test.describe "find"
            [ Test.test "Finds first list element that matches." <|
                \_ ->
                    Expect.equal
                        (find (\x -> modBy 2 x == 0) [ 3, 5, 4, 1, 2 ])
                        (Just 4)
            , Test.test "Fails if no list element matches." <|
                \_ ->
                    Expect.equal
                        (find (\x -> modBy 2 x == 0) [ 3, 5, 5, 1, 7 ])
                        Nothing
            , Test.test "Fails on empty list." <|
                \_ ->
                    Expect.equal
                        (find (\x -> modBy 2 x == 0) [])
                        Nothing
            ]
        , Test.describe "mapReduce"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (mapReduce (+) 0 String.length [ "hello", "in", "TV", "land" ])
                        13
            , Test.test "Can simulate `List.sum`." <|
                \_ ->
                    Expect.equal
                        (mapReduce (+) 0 identity [ 3, 4, -2, 1 ])
                        6
            , Test.test "More general than `String.concat`." <|
                \_ ->
                    Expect.equal
                        (mapReduce (++) "" String.fromInt [ 3, 4, 5, 23 ])
                        "34523"
            , Test.test "Returns neutral element on empty list." <|
                \_ ->
                    Expect.equal
                        (mapReduce (++) "" String.fromInt [])
                        ""
            ]
        , Test.describe "groupBy"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (groupBy String.length [ "hello", "in", "TV", "land" ])
                        (Dict.fromList [ ( 2, [ "in", "TV" ] ), ( 4, [ "land" ] ), ( 5, [ "hello" ] ) ])
            ]
        ]
