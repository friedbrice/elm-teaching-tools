module ElmTeachingTools.Labs.Life exposing
    ( Cell, CellStatus(..), Board
    , LifeRules
    , runLife
    )

{-| Conway's Game of Life.

For details, see the Wikipedia article,
<https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>.


# Basics

@docs Cell, CellStatus, Board


# Rules

@docs LifeRules


# Running

@docs runLife

-}

import ElmTeachingTools.Lib.Color as Color
import ElmTeachingTools.Lib.Game as Game
import ElmTeachingTools.Lib.Graphics as Graphics
import ElmTeachingTools.Lib.Keyboard as Keyboard
import ElmTeachingTools.Lib.List as List
import ElmTeachingTools.Lib.Memoize as Memoize


{-| A cell is a position in an infinite grid.
-}
type alias Cell =
    ( Int, Int )


{-| A cell is either `Alive` or `Dead`.
-}
type CellStatus
    = Dead
    | Alive


{-| Conceptually, a board is an infinite grid of cells, each of which
is either alive or dead.

We model this concept in our program using a **function**. Specifically,
a function that outputs a `CellStatus` when you input a `Cell`.

If you have a `Board` and `Cell`, you can find the status of that
`Cell` by plugging it into the `Board`.

If you need to create a board, you do so by writing a function whose
variable is a `Cell` and that returns a `CellStatus`.

-}
type alias Board =
    Cell -> CellStatus


{-| The rules we need to describe in order to make a working _Life_ game
can be modeled by these three functions:

  - `nextStatus : Int -> CellStatus -> CellStatus`
    Given the number of living neighbors and the current cell status, what
    should the cell's next status be? Use the rules listed in the Wikipedia
    article to write this function.

  - `livingNeighbors : Board -> Cell -> Int`
    Given the current board and a cell, how many living neighbors does that
    cell have? You'll need a tiny bit of geometry to write this function.

  - `nextBoard : Board -> Board`
    Given the current board, what should the next board be? You should use
    your `livingNeighbors` and `nextStatus` functions to write this function.

-}
type alias LifeRules =
    { nextStatus : Int -> CellStatus -> CellStatus
    , livingNeighbors : Board -> Cell -> Int
    , nextBoard : Board -> Board
    }


{-| To complete this lab, import this library into your own Elm file,
create an appropriate `LifeRules` record, and use it in `runLife`.

    module Main exposing (main)

    import ElmTeachingTools.Labs.Life exposing (..)

    myLifeRules : LifeRules
    myLifeRules =
        { -- your code here
        }

    main =
        runLife myLifeRules

-}
runLife : LifeRules -> Game.Game State Event
runLife rules =
    Game.game
        { init = initialState
        , update = updateGame rules
        , view = drawGame
        }


type alias State =
    { board : Board
    , paused : Bool
    }


type Event
    = ClickCell Cell


initialState : State
initialState =
    { board =
        \cell ->
            case cell of
                ( 1, 0 ) ->
                    Alive

                ( 2, 1 ) ->
                    Alive

                ( 0, 2 ) ->
                    Alive

                ( 1, 2 ) ->
                    Alive

                ( 2, 2 ) ->
                    Alive

                _ ->
                    Dead
    , paused = False
    }


memoBoard : Memoize.MemoizeStrategy Cell Cell CellStatus
memoBoard =
    { default = Dead
    , domain = allCells
    , fromKey = identity
    , toKey = identity
    }


updateGame : LifeRules -> Game.GameEvent Event -> State -> State
updateGame rules event state =
    let
        step =
            Memoize.memoize memoBoard << rules.nextBoard
    in
    case event of
        Game.ClockTick _ ->
            if state.paused then
                state

            else
                { state | board = step state.board }

        Game.Keyboard (Keyboard.KeyDown Keyboard.Key_Space) ->
            { state | paused = not state.paused }

        Game.Keyboard (Keyboard.KeyDown Keyboard.Key_RightArrow) ->
            if not state.paused then
                state

            else
                { state | board = step state.board }

        Game.Keyboard _ ->
            state

        Game.Custom (ClickCell cell) ->
            { state
                | board = flipCell cell state.board
            }


boardSize : Int
boardSize =
    50


allCells : List Cell
allCells =
    let
        range =
            List.range 0 boardSize
    in
    List.cross range range


drawGame : State -> Graphics.Screen Event
drawGame state =
    let
        drawCell : Cell -> Graphics.Graphic Event
        drawCell ( x, y ) =
            Graphics.rectangle
                { topLeft = ( toFloat x, toFloat y )
                , width = 1
                , height = 1
                , radius = 0
                , border = Nothing
                , fill =
                    Just <|
                        case state.board ( x, y ) of
                            Alive ->
                                Color.teal

                            Dead ->
                                Color.silver
                , onClick = Just (ClickCell ( x, y ))
                }
    in
    Graphics.screen
        { title = "Conway's Life"
        , widthPx = 500
        , heightPx = 500
        , xMax = toFloat boardSize
        , yMax = toFloat boardSize
        , children = List.map drawCell allCells
        }


flipStatus : CellStatus -> CellStatus
flipStatus status =
    case status of
        Alive ->
            Dead

        Dead ->
            Alive


flipCell : Cell -> Board -> Board
flipCell c0 b c =
    if c == c0 then
        flipStatus (b c)

    else
        b c
