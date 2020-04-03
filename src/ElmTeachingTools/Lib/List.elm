module ElmTeachingTools.Lib.List exposing
    ( iterate, unfold, fromMaybe
    , dropWhile, takeWhile, filterMaybes
    , zip, cross, join
    , inc, dec, find, mapReduce, groupBy
    )

{-| Extra functions to make working with lists easier.


# Create lists

@docs iterate, unfold, fromMaybe


# Transform lists

@docs dropWhile, takeWhile, filterMaybes


# Combine lists

@docs zip, cross, join


# Use lists

@docs inc, dec, find, mapReduce, groupBy

-}

import Dict exposing (Dict)
import ElmTeachingTools.Lib.Maybe as Maybe


{-| Create a list by repeatedly applying a function, stopping when the
function returns `Nothing`.

**Warning:** This function can cause an infinite loop (or, more likely,
a "Maximum call stack size exceeded" error).

    --| Example.
    iterate
        (\x ->
            if x == 3 then
                Nothing

            else
                Just (x + 1)
        )
        0
    --> [1, 2, 3]

-}
iterate : (a -> Maybe a) -> a -> List a
iterate f =
    unfold (\x -> f x |> Maybe.map (\y -> ( y, y )))


{-| A more general version of `iterate`. Create a list by repeatedly
applying a function, stopping when the function return `Nothing`.

**Warning:** This function can cause an infinite loop (or, more likely,
a "Maximum call stack size exceeded" error).

    --| Example.
    unfold String.uncons "slowly"
    --> ['s', 'l', 'o', 'w', 'l', 'y']

    --| Can simulate `iterate`.
    unfold
        (\x ->
            if x == 3 then
                Nothing

            else
                Just (x + 1, x + 1)
        )
        0
    --> [1, 2, 3]

    --| More general than `iterate`.
    unfold
        (\x ->
            if x == 3 then
                Nothing

            else
                Just (x^2, x + 1)
        )
        0
    --> [0, 1, 4]

-}
unfold : (b -> Maybe ( a, b )) -> b -> List a
unfold f b0 =
    case f b0 of
        Nothing ->
            []

        Just ( a, b1 ) ->
            a :: unfold f b1


{-| Create either a singleton list or an empty list from a `Maybe`.

    --| Empty maybe yields empty list.
    fromMaybe Nothing
    --> []

    --| Populated maybe yields singleton list.
    fromMaybe (Just 5)
    --> [5]

-}
fromMaybe : Maybe a -> List a
fromMaybe m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]


{-| Drop elements from the front of a list until the supplied callback is false.

    --| Example.
    dropWhile (\x -> x < 5) [ 1, 2, 3, 4, 5, 3 ]
    --> [5, 3]

-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile p list =
    case list of
        [] ->
            []

        x :: xs ->
            if p x then
                dropWhile p xs

            else
                x :: xs


{-| Take elements from the front of a list until the supplied callback is false.

    --| Example.
    takeWhile (\x -> x < 5) [ 1, 2, 3, 4, 5, 3 ]
    --> [1, 2, 3, 4]

-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
    let
        helper acc rest =
            case rest of
                [] ->
                    acc

                x :: xs ->
                    if p x then
                        helper (x :: acc) xs

                    else
                        acc
    in
    List.reverse (helper [] list)


{-| Filter `Nothing`s out of a list of `Maybe`s, unwrappig the `Just`s.

    --| Example.
    filterMaybes [ Just 1, Just 2, Nothing, Just 4 ]
    --> [1, 2, 4]

-}
filterMaybes : List (Maybe a) -> List a
filterMaybes =
    List.filterMap identity


{-| Create a list of pairs by lining two lists up.
If one list is longer, the extra elements are dropped.

    --| Example.
    zip [ 1, 2, 3 ] [ "a", "b", "c", "d" ]
    --> [(1, "a"), (2, "b"), (3, "c")]

-}
zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair


{-| Create the list of all possible pairs from two other lists.

    --| Example.
    cross [ 1, 2, 3 ] [ 'a', 'b' ]
    --> [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b')]

-}
cross : List a -> List b -> List ( a, b )
cross =
    join (\_ _ -> True)


{-| Create the list of all pairs that satisfy a condition.

    --| Example.
    join (\x y -> even (x + y)) [1, 2, 3] [4, 5, 6]
    --> [(1, 5), (2, 4), (2, 6), (3, 5)]

-}
join : (a -> b -> Bool) -> List a -> List b -> List ( a, b )
join p xs ys =
    xs
        |> List.concatMap
            (\x ->
                ys
                    |> List.concatMap
                        (\y ->
                            if p x y then
                                [ ( x, y ) ]

                            else
                                []
                        )
            )


{-| Select the list element following the value you provided, if one exists.

    --| Advance to next element of the list.
    inc [ "do", "re", "mi" ] "re"
    --> Just "mi"

    --| Fails when the element does not appear in the list.
    inc [ "do", "mi", "so" ] "re"
    --> Nothing

    --| Fails when the element appears only at the end of the list.
    inc [ "do", "re", "mi" ] "mi"
    --> Nothing

    --| Fails when the list is empty.
    inc [] "mi"
    --> Nothing

-}
inc : List a -> a -> Maybe a
inc enum elem =
    case enum of
        x1 :: x2 :: xs ->
            if x1 == elem then
                Just x2

            else
                inc (x2 :: xs) elem

        _ ->
            Nothing


{-| Select the list element preceeding the value you provided, if one exists.

    --| Go back to the previous element of the list.
    dec [ "do", "re", "mi" ] "re"
    --> Just "do"

    --| Fails when the element does not appear in the list.
    dec [ "do", "mi", "so" ] "re"
    --> Nothing

    --| Fails when the element appears at the beginning of the list.
    dec [ "do", "re", "mi" ] "do"
    --> Nothing

    --| Fails when the list is empty.
    dec [] "do"
    --> Nothing

-}
dec : List a -> a -> Maybe a
dec enum elem =
    case enum of
        x1 :: x2 :: xs ->
            if x2 == elem then
                Just x1

            else
                dec (x2 :: xs) elem

        _ ->
            Nothing


{-| Find the first element of the list that satisfies the predicate, or
an empty maybe if no list elements satisfy the predicate.

    --| Finds first list element that matches.
    find even [ 3, 5, 4, 1, 2 ]
    --> Just 4

    --| Fails if no list element matches.
    find even [ 3, 5, 5, 1, 7 ]
    --> Nothing

    --| Fails on empty list.
    find even []
    --> Nothing

-}
find : (a -> Bool) -> List a -> Maybe a
find p list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if p x then
                Just x

            else
                find p xs


{-| Provide a combining operation, a neutral element, and a mapping
function. Maps the mapping function over a list while reducing the
results using the combining function.

    --| Example.
    mapReduce (+) 0 String.length ["hello", "in", "TV", "land"]
    --> 13

    --| Can simulate `List.sum`.
    mapReduce (+) 0 identity [3, 4, -2, 1]
    --> 6

    --| More general than `String.concat`.
    mapReduce (++) "" String.fromInt [3, 4, 5, 23]
    --> "34523"

    --| Returns neutral element on empty list.
    mapReduce (++) "" String.fromInt []
    --> ""

-}
mapReduce : (b -> b -> b) -> b -> (a -> b) -> List a -> b
mapReduce mult e eval =
    List.foldl (\x acc -> mult acc (eval x)) e


{-| Provide a key function to partition a list into sublists by key.

    --| Example.
    groupBy String.length ["hello", "in", "TV", "land"]
    --> fromList [(2,["in","TV"]),(4,["land"]),(5,["hello"])]

-}
groupBy : (a -> comparable) -> List a -> Dict.Dict comparable (List a)
groupBy key =
    let
        f x =
            Dict.update (key x) (g x)

        g x y =
            case y of
                Nothing ->
                    Just [ x ]

                Just xs ->
                    Just (x :: xs)
    in
    Dict.map (\_ v -> List.reverse v) << List.foldl f Dict.empty
