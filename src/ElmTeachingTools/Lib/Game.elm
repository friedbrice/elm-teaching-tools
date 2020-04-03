module ElmTeachingTools.Lib.Game exposing
    ( Game, game
    , Timestamp, GameEvent(..)
    )

{-| A library with a small scaffolding for creating simple games.


# Creating games

@docs Game, game


# Game events

@docs Timestamp, GameEvent

-}

import Browser exposing (document)
import Browser.Events exposing (onAnimationFrame)
import ElmTeachingTools.Lib.Graphics exposing (Screen)
import ElmTeachingTools.Lib.Keyboard exposing (KeyEvent, keyEvents)
import Html
import Time exposing (posixToMillis)


{-| A simple game, with state type `s` and event type `e`.
-}
type alias Game s e =
    Program () s (GameEvent e)


{-| Create a game.

This follows the _State Machine_ program architecture. We must provide:

1.  A type `s` of possible program states,
2.  A type `e` of possible custom events,
3.  A starting state `init : s`,
4.  A transition function `update : GameEvent e -> s -> s`, and
5.  A view function `view : s -> Screen e`

This framework comes with some built-in events. Specifically,
`GameEvent` can be a `ClockTick` event, a `Keyboard` event,
or it can be a `Custom` event with type `e`.

-}
game :
    { init : s
    , update : GameEvent e -> s -> s
    , view : s -> Screen e
    }
    -> Game s e
game props =
    document
        { init = \_ -> ( props.init, Cmd.none )
        , update = \e s -> ( props.update e s, Cmd.none )
        , subscriptions =
            \_ ->
                let
                    clockTick =
                        onAnimationFrame (ClockTick << posixToMillis)

                    keyboard =
                        Sub.map Keyboard keyEvents
                in
                Sub.batch [ clockTick, keyboard ]
        , view =
            \s ->
                let
                    { title, body } =
                        props.view s

                    newBody =
                        List.map (Html.map Custom) body
                in
                { title = title, body = newBody }
        }


{-| The number of milliseconds since Midnight UTC Jan 1 1970.

**Note:** You can tell how much time has elapsed in your game by
saving the start time in your game state and then subtracting later on.

-}
type alias Timestamp =
    Int


{-| Events that your game can respond to.

A `ClockTick` event occurs about 60 times per second and carries the
current timestamp.

A `Keyboard` event occurs whenever a key is pushed down or released
(see `Lib.Keyboard`).

A `Custom` event can be triggered by the `onClick` property of a
`Graphic` element (see `Lib.Graphics`).

-}
type GameEvent e
    = ClockTick Timestamp
    | Keyboard KeyEvent
    | Custom e
