module ElmTeachingTools.Lib.Maybe exposing
    ( assert, filtered
    , cases
    , compose, paired, orElse, filter, traverse, sequence
    )

{-| Extra functions to make working with maybes easier.


# Create maybes

@docs assert, filtered


# Use maybes

@docs cases


# Transform maybes

@docs compose, paired, orElse, filter, traverse, sequence

-}


{-| Keep a value if a condition is met.

    --| Turns `True` into a `Just` value.
    assert True 5
    --> Just 5

    --| Turns `False` into `Nothing`
    assert False 5
    --> Nothing

-}
assert : Bool -> a -> Maybe a
assert p x =
    if p then
        Just x

    else
        Nothing


{-| Keep a value if it passes a predicate.

I like to think of this function as converting a predicate
`a -> Bool` into a more informative function `a -> Maybe a`.

    --| Rejects values that fail the predicate.
    filtered even 5
    --> Nothing

    --| Keeps values that pass the predicate.
    filtered even 6
    --> Just 6

-}
filtered : (a -> Bool) -> a -> Maybe a
filtered f x =
    if f x then
        Just x

    else
        Nothing


{-| Consume a maybe value by providing a callback to run if the value is
present and a fallback value in case it is not. Conceptually similar to
pattern matching, but in the form of a function.

    --| `Nothing` uses the fallback.
    Nothing |> cases "nope" String.fromInt
    --> "nope"

    --| A `Just` value uses the callback.
    Just 3 |> cases "nope" String.fromInt
    --> "3"

-}
cases : b -> (a -> b) -> Maybe a -> b
cases y f xs =
    case xs of
        Nothing ->
            y

        Just x ->
            f x


{-| Chain two functions that produce `Maybe`s together.

    --| Succeeds when both functions succeed.
    compose (filtered even) String.toInt "46"
    --> Just 46

    --| Fails when second function applied fails.
    compose (filtered even) String.toInt "47"
    --> Nothing

    --| Fails when first function applied fails.
    compose (filtered even) String.toInt "nope"
    --> Nothing

-}
compose : (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
compose f g x =
    g x |> Maybe.andThen f


{-| Create a pair from two `Maybe`s, giving `Nothing` if either is `Nothing`.

    --| Fails when second argument is `Nothing`.
    paired (Just 4) Nothing
    --> Nothing

    --| Fails when first argument is `Nothing`.
    paired Nothing (Just 5)
    --> Nothing

    --| Succeeds when both arguments are `Just` values.
    paired (Just 4) (Just 5)
    --> Just (4, 5)

-}
paired : Maybe a -> Maybe b -> Maybe ( a, b )
paired =
    Maybe.map2 Tuple.pair


{-| Replace a maybe with a fallback in case it is empty.

**Note:** `orElse Nothing` is a no-op.

    --| Keeps first `Just` value.
    Just 5 |> orElse (Just 3)
    --> Just 5

    --| Uses fallback when first value is `Nothing`.
    Nothing |> orElse (Just 3)
    --> Just 3

    --| No-op on `Just` values.
    Just 3 |> orElse Nothing
    --> Just 3

    --| No-op on `Nothing`.
    Nothing |> orElse Nothing
    --> Nothing

-}
orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback primary =
    case primary of
        Just _ ->
            primary

        Nothing ->
            fallback


{-| Keep a maybe value if it satisfies the predicate.

    --| Fails on `Nothing`.
    filter (\x -> modBy 2 x == 0) Nothing
    --> Nothing

    --| Succeeds on `Just` values that pass the predicate.
    filter (\x -> modBy 2 x == 0) (Just 4)
    --> Just 4

    --| Fails on `Just` values that fail the predicate.
    filter (\x -> modBy 2 x == 0) (Just 3)
    --> Nothing

-}
filter : (a -> Bool) -> Maybe a -> Maybe a
filter p m =
    m
        |> Maybe.andThen
            (\a ->
                if p a then
                    m

                else
                    Nothing
            )


{-| Map a function that returns `Maybe`s over a list. The result is
`Nothing` when the mapping function returns `Nothing` on at least one
list element.

    --| Succeeds on empty list.
    traverse String.toInt []
    --> Just []

    --| Succeeds when all list elements succeed.
    traverse String.toInt ["2", "3", "4"]
    --> Just [2, 3, 4]

    --| Fails when any list element fails.
    traverse String.toInt ["2", "lolwut", "4"]
    --> Nothing

-}
traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        reduce x acc =
            case f x of
                Nothing ->
                    Nothing

                Just y ->
                    Maybe.map (\ys -> y :: ys) acc
    in
    Maybe.map List.reverse << List.foldl reduce (Just [])


{-| Convert a list of `Maybe`s into a `Maybe` list. The result is
`Nothing` when at least one of the list elements is `Nothing`.

    --| Succeeds on empty list.
    sequence []
    --> Just []

    --| Succeeds when all list elements are `Just` values.
    sequence [Just 2, Just 3, Just 4]
    --> Just [2, 3, 4]

    --| Fails when any list element is `Nothing`.
    sequence [Just 2, Nothing, Just 4]
    --> Nothing

-}
sequence : List (Maybe a) -> Maybe (List a)
sequence =
    traverse identity
