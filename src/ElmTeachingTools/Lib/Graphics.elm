module ElmTeachingTools.Lib.Graphics exposing
    ( Graphic, rectangle, circle, line, curve
    , Screen, screen
    )

{-| A library with functions for putting graphics on the screen.


# Graphical elements

@docs Graphic, rectangle, circle, line, curve


# Screen

@docs Screen, screen

-}

import Browser
import ElmTeachingTools.Lib.Color exposing (Color, hexValue)
import ElmTeachingTools.Lib.String as String
import Svg
import Svg.Attributes as Attr
import Svg.Events as Events


{-| A "scaled vector graphic" that can be drawn on a `Screen`.
-}
type alias Graphic e =
    Svg.Svg e


{-| -}
rectangle :
    { topLeft : ( Float, Float )
    , width : Float
    , height : Float
    , radius : Float
    , border : Maybe ( Float, Color )
    , fill : Maybe Color
    , onClick : Maybe e
    }
    -> Graphic e
rectangle props =
    mkGfx Svg.rect
        props.border
        props.fill
        props.onClick
        [ Attr.x (String.fromFloat (Tuple.first props.topLeft))
        , Attr.y (String.fromFloat (Tuple.second props.topLeft))
        , Attr.width (String.fromFloat props.width)
        , Attr.height (String.fromFloat props.height)
        , Attr.rx (String.fromFloat props.radius)
        ]


{-| -}
circle :
    { center : ( Float, Float )
    , radius : Float
    , border : Maybe ( Float, Color )
    , fill : Maybe Color
    , onClick : Maybe e
    }
    -> Graphic e
circle props =
    mkGfx Svg.circle
        props.border
        props.fill
        props.onClick
        [ Attr.cx (String.fromFloat (Tuple.first props.center))
        , Attr.cy (String.fromFloat (Tuple.second props.center))
        , Attr.r (String.fromFloat props.radius)
        ]


{-| -}
line :
    { start : ( Float, Float )
    , end : ( Float, Float )
    , border : ( Float, Color )
    , onClick : Maybe e
    }
    -> Graphic e
line props =
    mkGfx Svg.path
        (Just props.border)
        Nothing
        props.onClick
        [ mkPath <| (M props.start <| L props.end <| N) ]


{-| -}
curve :
    { start : ( Float, Float )
    , end : ( Float, Float )
    , control : ( Float, Float )
    , border : ( Float, Color )
    , onClick : Maybe e
    }
    -> Graphic e
curve props =
    mkGfx Svg.path
        (Just props.border)
        Nothing
        props.onClick
        [ mkPath <| (M props.start <| Q props.control props.end <| N) ]


{-| A web page consisting of a screen onto which `Graphic`s can be drawn.
-}
type alias Screen e =
    Browser.Document e


{-| Create a `Screen`.
-}
screen :
    { title : String
    , widthPx : Int
    , heightPx : Int
    , xMax : Float
    , yMax : Float
    , children : List (Graphic e)
    }
    -> Screen e
screen props =
    { title = props.title
    , body =
        [ Svg.svg
            [ Attr.width (String.fromInt props.widthPx)
            , Attr.height (String.fromInt props.heightPx)
            , Attr.viewBox <|
                let
                    x =
                        String.fromFloat props.xMax

                    y =
                        String.fromFloat props.yMax
                in
                "0 0 " ++ x ++ " " ++ y
            ]
            props.children
        ]
    }


mkGfx :
    (List (Svg.Attribute e) -> List (Svg.Svg e) -> Svg.Svg e)
    -> Maybe ( Float, Color )
    -> Maybe Color
    -> Maybe e
    -> List (Svg.Attribute e)
    -> Graphic e
mkGfx svg border fill onClick attrs =
    let
        borderAttrs =
            case border of
                Nothing ->
                    []

                Just ( width, color ) ->
                    [ Attr.strokeWidth (String.fromFloat width)
                    , Attr.stroke (hexValue color)
                    ]

        fillAttr =
            case fill of
                Nothing ->
                    [ Attr.fillOpacity "0" ]

                Just color ->
                    [ Attr.fill (hexValue color) ]

        onClickAttr =
            case onClick of
                Nothing ->
                    []

                Just event ->
                    [ Events.onClick event ]
    in
    svg (attrs ++ borderAttrs ++ fillAttr ++ onClickAttr) []


type Path
    = M ( Float, Float ) Path
    | L ( Float, Float ) Path
    | Q ( Float, Float ) ( Float, Float ) Path
    | T ( Float, Float ) Path
    | C ( Float, Float ) ( Float, Float ) ( Float, Float ) Path
    | S ( Float, Float ) ( Float, Float ) Path
    | Z
    | N


mkPath : Path -> Svg.Attribute e
mkPath =
    let
        pt ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        sp =
            String.format { prefix = "", infix = " ", suffix = "" } identity

        render p0 =
            case p0 of
                M xy p ->
                    "M" ++ sp [ pt xy, render p ]

                L xy p ->
                    "L" ++ sp [ pt xy, render p ]

                Q xy1 xy2 p ->
                    "Q" ++ sp [ pt xy1, pt xy2, render p ]

                T xy p ->
                    "T" ++ sp [ pt xy, render p ]

                C xy1 xy2 xy3 p ->
                    "Q" ++ sp [ pt xy1, pt xy2, pt xy3, render p ]

                S xy1 xy2 p ->
                    "S" ++ sp [ pt xy1, pt xy2, render p ]

                Z ->
                    "Z"

                N ->
                    ""
    in
    Attr.d << render
