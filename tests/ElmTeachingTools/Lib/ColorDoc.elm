module ElmTeachingTools.Lib.ColorDoc exposing (..)

import ElmTeachingTools.Lib.Color exposing (..)
import Expect
import Test
import TestCommon exposing (..)


suite : Test.Test
suite =
    Test.describe "ElmTeachingTools.Lib.Color"
        [ Test.describe "hexValue"
            [ Test.test "Example" <|
                \_ ->
                    Expect.equal
                        (hexValue (Color "#c0c0c0"))
                        "#c0c0c0"
            ]
        ]
