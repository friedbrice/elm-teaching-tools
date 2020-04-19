module ElmTeachingTools.Labs.Introduction exposing
    ( runIntroduction
    , Exercises
    )

{-| Some exercises to get you familiar with programming in Elm.

@docs runIntroduction

@docs Exercises

-}

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser
import ElmTeachingTools.Lib.Maybe as Maybe
import ElmTeachingTools.Lib.String as String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| This lab consists of six exercises to help you get started learning
the basics of Elm and how to use it to write interactive web programs.

  - `ex0_sayHello : String -> String`
    Give someone a nice, name-appropriate greeting.
    If their name ends with "y", give them a friendly "Hey, <name>!".
    If their name doesn't end with "y", give a more formal "Hello, <name>.".
    If they leave their name blank, kindly ask "Hello, who's there?".

  - `ex1_countVowels : String -> Int`
    Count the number of vowels in the input.
    For the purposes of this exercise, the letters "a", "e", "i", "o",
    and "u" are considered vowels.
    (Upper-case versions are also vowels!)

  - `ex2_toCamelCase : String -> String`
    Convert a sentence to camelCase.
    Capitalize the first letter of each word except the first,
    make sure all other letters are lower case,
    and remove all spaces.

  - `ex3_diffList : List Int -> Maybe (List Int)`
    Given a list of positive integers,
    compute the list of differences between successive terms.
    Notice, this list will be one shorter than that starting list.
    If the user gives only one integer, return an empty list.
    If the user gives empty input, return `Nothing`".

  - `ex4_partialSums : List Int -> List Int`
    Given a list of positive integers,
    compute the list of partial sums, starting with 0.
    Notice your list will be one longer than the input list.
    If the user gives empty input, return the list `[0]`.

  - `ex5_simpleCalc : String -> String`
    Make a calculator capable of addition and multiplication of integers.
    Follow the ordinary order of operations, multiplication then addition.
    A `-` sign will always mean "negative" and never mean subtraction.
    If the input has any characters other than digits, `*`, `+`, and `-`,
    return the string, "Sorry, bad input!".

-}
type alias Exercises =
    { ex0_sayHello : String -> String
    , ex1_countVowels : String -> Int
    , ex2_toCamelCase : String -> String
    , ex3_diffList : List Int -> Maybe (List Int)
    , ex4_partialSums : List Int -> List Int
    , ex5_simpleCalc : String -> String
    }


{-| To complete this lab, import this library in your own file,
create an appropriate `Exercises` value, and use your `Exercises` value
with `runIntroduction`.

    module Main exposing (main)

    import ElmTeachingTools.Labs.Introduction exposing (..)

    myExercises : Exercises
    myExercises =
        { -- your code here
        }

    main =
        runIntroduction myExercises

-}
runIntroduction : Exercises -> Program () Model Message
runIntroduction ex =
    Browser.sandbox
        { init = Model E0 "<INPUT>" "<OUTPUT>" []
        , update = update ex
        , view = view ex
        }


type Exercise
    = E0
    | E1
    | E2
    | E3
    | E4
    | E5


type Message
    = SetExercise Exercise
    | SetInput String
    | TryInput
    | RunTests


type alias Model =
    { exercise : Exercise
    , input : String
    , output : String
    , testResults : List TestResult
    }


type alias TestResult =
    { input : String
    , expected : String
    , actual : String
    , pass : Bool
    }


update : Exercises -> Message -> Model -> Model
update ex action state =
    case action of
        SetExercise newExercise ->
            { state | exercise = newExercise }

        SetInput newInput ->
            { state | input = newInput }

        TryInput ->
            let
                ( solve, _ ) =
                    dispatch ex state.exercise

                newOutput =
                    solve state.input
            in
            { state | output = newOutput }

        RunTests ->
            let
                ( solve, tests ) =
                    dispatch ex state.exercise

                runTest ( x, y ) =
                    TestResult x y (solve x) (y == solve x)

                results =
                    List.map runTest tests
            in
            { state | testResults = results }


dispatch : Exercises -> Exercise -> ( String -> String, List ( String, String ) )
dispatch ex exercise =
    case exercise of
        E0 ->
            ( ex.ex0_sayHello
            , sayHelloTests
            )

        E1 ->
            ( String.fromInt << ex.ex1_countVowels
            , List.map
                (\( x, y ) ->
                    ( x, String.fromInt y )
                )
                countVowelsTests
            )

        E2 ->
            ( ex.ex2_toCamelCase
            , toCamelCaseTests
            )

        E3 ->
            ( Maybe.cases "Sorry, bad input!" String.fromInts
                << Maybe.compose ex.ex3_diffList String.toInts
            , List.map
                (\( x, y ) ->
                    ( String.fromInts x, String.fromInts x )
                )
                diffListTests
            )

        E4 ->
            ( Maybe.cases
                "Sorry, bad input!"
                (String.fromInts << ex.ex4_partialSums)
                << String.toInts
            , List.map
                (\( x, y ) ->
                    ( String.fromInts x, String.fromInts y )
                )
                partialSumsTests
            )

        E5 ->
            ( ex.ex5_simpleCalc
            , simpleCalcTests
            )


view : Exercises -> Model -> Html Message
view ex model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row
            [ Row.centerXs ]
            [ Grid.col [ Col.xs9 ] (heading model) ]
        , Grid.row
            [ Row.centerXs ]
            [ Grid.col [ Col.xs2 ] (sidebar model)
            , Grid.col [ Col.xs7 ]
                [ Grid.row [] [ Grid.col [] (inputPane model) ]
                , Grid.row [] [ Grid.col [] (outputPane model) ]
                , Grid.row [] [ Grid.col [] (testResultsPane model) ]
                ]
            ]
        ]


heading : Model -> List (Html Message)
heading _ =
    [ h1 [] [ text "Math 137 - Workshop 1" ] ]


sidebar : Model -> List (Html Message)
sidebar model =
    [ Html.map SetExercise <|
        radiolist "exercise"
            model.exercise
            [ ( "Exercise 0", E0 )
            , ( "Exercise 1", E1 )
            , ( "Exercise 2", E2 )
            , ( "Exercise 3", E3 )
            , ( "Exercise 4", E4 )
            , ( "Exercise 5", E5 )
            ]
    , Html.button [ onClick TryInput ] [ text "TryInput!" ]
    , Html.button [ onClick RunTests ] [ text "Run tests!" ]
    ]


inputPane : Model -> List (Html Message)
inputPane model =
    [ div [] [ strong [] [ text "Input:" ] ]
    , textarea
        [ cols 40, rows 12, onInput SetInput ]
        [ text model.input ]
    ]


outputPane : Model -> List (Html Message)
outputPane model =
    [ div [] [ strong [] [ text "Output:" ] ]
    , textarea
        [ cols 40, rows 4, readonly True ]
        [ text model.output ]
    ]


testResultsPane : Model -> List (Html Message)
testResultsPane model =
    let
        summary =
            case List.isEmpty model.testResults of
                True ->
                    ""

                False ->
                    case List.all .pass model.testResults of
                        True ->
                            "All tests passed!"

                        False ->
                            "Test failures."

        spreadRow r =
            [ r.input, r.expected, r.actual, showBool r.pass ]

        showBool x =
            case x of
                True ->
                    "Passed"

                False ->
                    "FAILED"
    in
    [ div [] [ strong [] [ text "Test Results: " ], em [] [ text summary ] ]
    , tabular
        [ "Input", "Expected", "Actual", "Result" ]
        (List.map spreadRow model.testResults)
    ]


radiolist : String -> a -> List ( String, a ) -> Html a
radiolist fieldName selected options =
    let
        mkbutton ( label, option ) =
            div []
                [ input
                    [ type_ "radio"
                    , name fieldName
                    , onClick option
                    , checked (option == selected)
                    ]
                    []
                , text label
                ]
    in
    div [] (List.map mkbutton options)


tabular : List String -> List (List String) -> Html a
tabular header rows =
    let
        makeRow xs =
            tr [] (List.map (\x -> td [] [ text x ]) xs)

        makeHeader xs =
            tr [] (List.map (\x -> td [] [ em [] [ text x ] ]) xs)
    in
    table [] ([ makeHeader header ] ++ List.map makeRow rows)


sayHelloTests : List ( String, String )
sayHelloTests =
    [ ( "Daniel", "Hello, Daniel." )
    , ( "Danny", "Hey, Danny!" )
    , ( "Victoria", "Hello, Victoria." )
    , ( "Tory", "Hey, Tory!" )
    , ( "Vicky", "Hey, Vicky!" )
    , ( "", "Hello, who's there?" )
    ]


countVowelsTests : List ( String, Int )
countVowelsTests =
    [ ( "OF2TWstbwQ3hejfY", 2 )
    , ( "The quick brown fox jumps over the lazy dog.", 11 )
    , ( "Lorem ipsum dolor sit amet.", 9 )
    , ( "What other means were you thinking?", 11 )
    , ( "15% off 15.00 is 12.75. The difference is 2.25", 8 )
    ]


toCamelCaseTests : List ( String, String )
toCamelCaseTests =
    [ ( "All along the watchtower", "allAlongTheWatchtower" )
    , ( "cRyPtO Is teH nEw gOlD", "cryptoIsTehNewGold" )
    , ( "double quotes for a String", "doubleQuotesForAString" )
    , ( "single quotes for a Char", "singleQuotesForAChar" )
    ]


diffListTests : List ( List Int, Maybe (List Int) )
diffListTests =
    [ ( [ 3 ], Just [] )
    , ( [], Nothing )
    , ( [ 1, 3 ], Just [ 2 ] )
    , ( [ 3, 2, 6, 4, 3 ], Just [ -1, 4, -2, -1 ] )
    ]


partialSumsTests : List ( List Int, List Int )
partialSumsTests =
    [ ( [], [ 0 ] )
    , ( [ 1, 3 ], [ 0, 1, 4 ] )
    , ( [ 3, 2, 6, 4, 3 ], [ 0, 3, 5, 11, 15, 18 ] )
    ]


simpleCalcTests : List ( String, String )
simpleCalcTests =
    [ ( "1", "1" )
    , ( "", "Sorry, bad input!" )
    , ( "1+2", "3" )
    , ( "1+2*3", "7" )
    , ( "2*4+3*7", "29" )
    , ( "3*100+5*10+8*1", "358" )
    , ( "(1+2)*3", "Sorry, bad input!" )
    , ( "1+x*3", "Sorry, bad input!" )
    , ( "1 + 2*3", "Sorry, bad input!" )
    , ( "1+-2*3", "-5" )
    , ( "1+2*-3", "-5" )
    , ( "-1+2*3", "5" )
    ]
