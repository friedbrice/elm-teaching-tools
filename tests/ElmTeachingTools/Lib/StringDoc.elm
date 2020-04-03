module ElmTeachingTools.Lib.StringDoc exposing (..)

import ElmTeachingTools.Lib.String exposing (..)
import ElmTeachingTools.TestCommon exposing (..)
import Expect
import Test


suite : Test.Test
suite =
    Test.describe "ElmTeachingTools.Lib.String"
        [ Test.describe "toInts"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (toInts "2,3,4,-1")
                        (Just [ 2, 3, 4, -1 ])
            , Test.test "Entire parse fails if parsing any element fails." <|
                \_ ->
                    Expect.equal
                        (toInts "2,3,lolwut,-1")
                        Nothing
            ]
        , Test.describe "fromInts"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (fromInts [ 2, 3, 4, -1 ])
                        "2,3,4,-1"
            ]
        , Test.describe "format"
            [ Test.test "Example." <|
                \_ ->
                    Expect.equal
                        (format { prefix = "<", infix = ":", suffix = ">" } String.fromInt [ 3, 4, 5 ])
                        "<3:4:5>"
            ]
        ]
