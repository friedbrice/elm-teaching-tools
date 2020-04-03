module ElmTeachingTools.TestCommon exposing (..)

import Dict


even : Int -> Bool
even x =
    modBy 2 x == 0


fromList : List ( comparable, a ) -> Dict.Dict comparable a
fromList =
    Dict.fromList
