module ElmTeachingTools.Lib.Color exposing
    ( Color(..), hexValue
    , white
    , silver
    , gray
    , black
    , red
    , maroon
    , yellow
    , olive
    , lime
    , green
    , aqua
    , teal
    , blue
    , navy
    , fuchsia
    , purple
    )

{-| A library with some stock colors so that you don't have to remember
their Hex values.


# Basic API

@docs Color, hexValue


# Predefined Colors

@docs white
@docs silver
@docs gray
@docs black
@docs red
@docs maroon
@docs yellow
@docs olive
@docs lime
@docs green
@docs aqua
@docs teal
@docs blue
@docs navy
@docs fuchsia
@docs purple

-}


{-| A color in RGB hex notation.
-}
type Color
    = Color String


{-| Get the RGB hex notation of a `Color`.

    --| Example
    hexValue (Color "#c0c0c0")
    --> "#c0c0c0"

-}
hexValue : Color -> String
hexValue (Color s) =
    s


{-| -}
white : Color
white =
    Color "#ffffff"


{-| -}
silver : Color
silver =
    Color "#c0c0c0"


{-| -}
gray : Color
gray =
    Color "#808080"


{-| -}
black : Color
black =
    Color "#000000"


{-| -}
red : Color
red =
    Color "#ff0000"


{-| -}
maroon : Color
maroon =
    Color "#800000"


{-| -}
yellow : Color
yellow =
    Color "#ffff00"


{-| -}
olive : Color
olive =
    Color "#808000"


{-| -}
lime : Color
lime =
    Color "#00ff00"


{-| -}
green : Color
green =
    Color "#008000"


{-| -}
aqua : Color
aqua =
    Color "#00ffff"


{-| -}
teal : Color
teal =
    Color "#008080"


{-| -}
blue : Color
blue =
    Color "#0000ff"


{-| -}
navy : Color
navy =
    Color "#000080"


{-| -}
fuchsia : Color
fuchsia =
    Color "#ff00ff"


{-| -}
purple : Color
purple =
    Color "#800080"
