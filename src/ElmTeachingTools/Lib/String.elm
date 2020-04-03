module ElmTeachingTools.Lib.String exposing (toInts, fromInts, format)

{-| Extra functions to make working with strings easier.

@docs toInts, fromInts, format

-}

import ElmTeachingTools.Lib.Maybe as Maybe


{-| Parse a comma-separated list of ints.

    --| Example.
    toInts "2,3,4,-1"
    --> Just [2, 3, 4, -1]

    --| Entire parse fails if parsing any element fails.
    toInts "2,3,lolwut,-1"
    --> Nothing

-}
toInts : String -> Maybe (List Int)
toInts =
    Maybe.traverse String.toInt << String.split ","


{-| Print a comma-separated list of ints.

    --| Example.
    fromInts [2, 3, 4, -1]
    --> "2,3,4,-1"

-}
fromInts : List Int -> String
fromInts =
    format { prefix = "", infix = ",", suffix = "" } String.fromInt


{-| Format a list into a string.

    --| Example.
    format { prefix = "<", infix = ":", suffix = ">" } String.fromInt [3,4,5]
    --> "<3:4:5>"

-}
format :
    { prefix : String
    , infix : String
    , suffix : String
    }
    -> (a -> String)
    -> List a
    -> String
format { prefix, infix, suffix } fmt =
    List.map fmt
        >> List.intersperse infix
        >> String.concat
        >> (\s -> prefix ++ s ++ suffix)
