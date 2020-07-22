module ElmTeachingTools.Lib.MaybeDoc exposing (..)

import ElmTeachingTools.Lib.Maybe exposing (..)
import Expect
import Test
import TestCommon exposing (..)


suite : Test.Test
suite =
    Test.describe "ElmTeachingTools.Lib.Maybe"
        [ Test.describe "assert"
            [ Test.test "Turns `True` into a `Just` value." <|
                \_ ->
                    Expect.equal
                        (assert True 5)
                        (Just 5)
            , Test.test "Turns `False` into `Nothing`" <|
                \_ ->
                    Expect.equal
                        (assert False 5)
                        Nothing
            ]
        , Test.describe "filtered"
            [ Test.test "Rejects values that fail the predicate." <|
                \_ ->
                    Expect.equal
                        (filtered even 5)
                        Nothing
            , Test.test "Keeps values that pass the predicate." <|
                \_ ->
                    Expect.equal
                        (filtered even 6)
                        (Just 6)
            ]
        , Test.describe "cases"
            [ Test.test "`Nothing` uses the fallback." <|
                \_ ->
                    Expect.equal
                        (Nothing |> cases "nope" String.fromInt)
                        "nope"
            , Test.test "A `Just` value uses the callback." <|
                \_ ->
                    Expect.equal
                        (Just 3 |> cases "nope" String.fromInt)
                        "3"
            ]
        , Test.describe "compose"
            [ Test.test "Succeeds when both functions succeed." <|
                \_ ->
                    Expect.equal
                        (compose (filtered even) String.toInt "46")
                        (Just 46)
            , Test.test "Fails when second function applied fails." <|
                \_ ->
                    Expect.equal
                        (compose (filtered even) String.toInt "47")
                        Nothing
            , Test.test "Fails when first function applied fails." <|
                \_ ->
                    Expect.equal
                        (compose (filtered even) String.toInt "nope")
                        Nothing
            ]
        , Test.describe "paired"
            [ Test.test "Fails when second argument is `Nothing`." <|
                \_ ->
                    Expect.equal
                        (paired (Just 4) Nothing)
                        Nothing
            , Test.test "Fails when first argument is `Nothing`." <|
                \_ ->
                    Expect.equal
                        (paired Nothing (Just 5))
                        Nothing
            , Test.test "Succeeds when both arguments are `Just` values." <|
                \_ ->
                    Expect.equal
                        (paired (Just 4) (Just 5))
                        (Just ( 4, 5 ))
            ]
        , Test.describe "orElse"
            [ Test.test "Keeps first `Just` value." <|
                \_ ->
                    Expect.equal
                        (Just 5 |> orElse (Just 3))
                        (Just 5)
            , Test.test "Uses fallback when first value is `Nothing`." <|
                \_ ->
                    Expect.equal
                        (Nothing |> orElse (Just 3))
                        (Just 3)
            , Test.test "No-op on `Just` values." <|
                \_ ->
                    Expect.equal
                        (Just 3 |> orElse Nothing)
                        (Just 3)
            , Test.test "No-op on `Nothing`." <|
                \_ ->
                    Expect.equal
                        (Nothing |> orElse Nothing)
                        Nothing
            ]
        , Test.describe "filter"
            [ Test.test "Fails on `Nothing`." <|
                \_ ->
                    Expect.equal
                        (filter (\x -> modBy 2 x == 0) Nothing)
                        Nothing
            , Test.test "Succeeds on `Just` values that pass the predicate." <|
                \_ ->
                    Expect.equal
                        (filter (\x -> modBy 2 x == 0) (Just 4))
                        (Just 4)
            , Test.test "Fails on `Just` values that fail the predicate." <|
                \_ ->
                    Expect.equal
                        (filter (\x -> modBy 2 x == 0) (Just 3))
                        Nothing
            ]
        , Test.describe "traverse"
            [ Test.test "Succeeds on empty list." <|
                \_ ->
                    Expect.equal
                        (traverse String.toInt [])
                        (Just [])
            , Test.test "Succeeds when all list elements succeed." <|
                \_ ->
                    Expect.equal
                        (traverse String.toInt [ "2", "3", "4" ])
                        (Just [ 2, 3, 4 ])
            , Test.test "Fails when any list element fails." <|
                \_ ->
                    Expect.equal
                        (traverse String.toInt [ "2", "lolwut", "4" ])
                        Nothing
            ]
        , Test.describe "sequence"
            [ Test.test "Succeeds on empty list." <|
                \_ ->
                    Expect.equal
                        (sequence [])
                        (Just [])
            , Test.test "Succeeds when all list elements are `Just` values." <|
                \_ ->
                    Expect.equal
                        (sequence [ Just 2, Just 3, Just 4 ])
                        (Just [ 2, 3, 4 ])
            , Test.test "Fails when any list element is `Nothing`." <|
                \_ ->
                    Expect.equal
                        (sequence [ Just 2, Nothing, Just 4 ])
                        Nothing
            ]
        ]
