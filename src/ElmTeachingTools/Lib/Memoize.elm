module ElmTeachingTools.Lib.Memoize exposing (MemoizeStrategy, memoize)

{-| Optimize program performance by converting a heavily-used function
into a data structure.

@docs MemoizeStrategy, memoize

-}

import Dict exposing (Dict)


{-| A strategy for turning a function into data structure.

**Note:** You should define any `MemoizeStrategy`s at the top-level of
a file so that you're guaranteed to neven recompute the domain.

-}
type alias MemoizeStrategy c x y =
    { toKey : x -> c
    , fromKey : c -> x
    , domain : List x
    , default : y
    }


{-| Converts a function into a data structure, and outputs a new
function that uses that data structure instead of the old function.
Sometimes useful for enhancing program performance.

**Note:** The new function will agree with the old function only on
the domain provided in the `MemoizeStrategy`. Outside of that domain,
the new function will return the `default` provided in the `MemoizeStrategy`.

-}
memoize : MemoizeStrategy comparable x y -> (x -> y) -> (x -> y)
memoize { toKey, fromKey, domain, default } fn =
    let
        keys =
            List.map toKey domain

        store =
            stow keys (fn << fromKey)
    in
    unstow default store << toKey


stow : List comparable -> (comparable -> a) -> Dict comparable a
stow domain f =
    Dict.fromList (List.map (\x -> ( x, f x )) domain)


unstow : a -> Dict comparable a -> (comparable -> a)
unstow default dict x =
    Maybe.withDefault default (Dict.get x dict)
