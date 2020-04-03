module ElmTeachingTools.Lib.Keyboard exposing (KeyEvent(..), Key(..), keyEvents)

{-| A library for detecting key presses and releases.

@docs KeyEvent, Key, keyEvents

-}

import Browser.Events
import Json.Decode exposing (andThen, fail, field, string, succeed)


{-| An event that triggers whenever a key is pressed down or released.
-}
type KeyEvent
    = KeyDown Key
    | KeyUp Key


{-| A `Key` represents a physical key on the keyboard, not a character.
In particular, a `Key` does not distinguish between lower case and upper
case.
-}
type Key
    = Key_Escape
    | Key_F1
    | Key_F2
    | Key_F3
    | Key_F4
    | Key_F5
    | Key_F6
    | Key_F7
    | Key_F8
    | Key_F9
    | Key_F10
    | Key_F11
    | Key_F12
    | Key_Insert
    | Key_Delete
    | Key_Home
    | Key_End
    | Key_PageUp
    | Key_PageDown
    | Key_UpArrow
    | Key_LeftArrow
    | Key_DownArrow
    | Key_RightArrow
    | Key_Tab
    | Key_CapsLock
    | Key_Shift
    | Key_Control
    | Key_Alt
    | Key_Backspace
    | Key_Enter
    | Key_Space
    | Key_Backtick
    | Key_Hyphen
    | Key_Equals
    | Key_LeftBracket
    | Key_RightBracket
    | Key_Backslash
    | Key_Semicolon
    | Key_Apostrophe
    | Key_Comma
    | Key_Period
    | Key_Slash
    | Key_1
    | Key_2
    | Key_3
    | Key_4
    | Key_5
    | Key_6
    | Key_7
    | Key_8
    | Key_9
    | Key_0
    | Key_Q
    | Key_W
    | Key_E
    | Key_R
    | Key_T
    | Key_Y
    | Key_U
    | Key_I
    | Key_O
    | Key_P
    | Key_A
    | Key_S
    | Key_D
    | Key_F
    | Key_G
    | Key_H
    | Key_J
    | Key_K
    | Key_L
    | Key_Z
    | Key_X
    | Key_C
    | Key_V
    | Key_B
    | Key_N
    | Key_M


{-| Subscribe to keyboard events.
-}
keyEvents : Sub KeyEvent
keyEvents =
    let
        keyDecoder =
            field "key" string |> andThen parseKey

        parseKey str =
            case str of
                "Escape" ->
                    succeed Key_Escape

                "F1" ->
                    succeed Key_F1

                "F2" ->
                    succeed Key_F2

                "F3" ->
                    succeed Key_F3

                "F4" ->
                    succeed Key_F4

                "F5" ->
                    succeed Key_F5

                "F6" ->
                    succeed Key_F6

                "F7" ->
                    succeed Key_F7

                "F8" ->
                    succeed Key_F8

                "F9" ->
                    succeed Key_F9

                "F10" ->
                    succeed Key_F10

                "F11" ->
                    succeed Key_F11

                "F12" ->
                    succeed Key_F12

                "Insert" ->
                    succeed Key_Insert

                "Delete" ->
                    succeed Key_Delete

                "Home" ->
                    succeed Key_Home

                "End" ->
                    succeed Key_End

                "PageUp" ->
                    succeed Key_PageUp

                "PageDown" ->
                    succeed Key_PageDown

                "ArrowUp" ->
                    succeed Key_UpArrow

                "ArrowLeft" ->
                    succeed Key_LeftArrow

                "ArrowDown" ->
                    succeed Key_DownArrow

                "ArrowRight" ->
                    succeed Key_RightArrow

                "Tab" ->
                    succeed Key_Tab

                "CapsLock" ->
                    succeed Key_CapsLock

                "Shift" ->
                    succeed Key_Shift

                "Control" ->
                    succeed Key_Control

                "Alt" ->
                    succeed Key_Alt

                "Backspace" ->
                    succeed Key_Backspace

                "Enter" ->
                    succeed Key_Enter

                " " ->
                    succeed Key_Space

                "`" ->
                    succeed Key_Backtick

                "~" ->
                    succeed Key_Backtick

                "-" ->
                    succeed Key_Hyphen

                "_" ->
                    succeed Key_Hyphen

                " =" ->
                    succeed Key_Equals

                "+" ->
                    succeed Key_Equals

                "[" ->
                    succeed Key_LeftBracket

                "{" ->
                    succeed Key_LeftBracket

                "]" ->
                    succeed Key_RightBracket

                "}" ->
                    succeed Key_RightBracket

                "\\" ->
                    succeed Key_Backslash

                "|" ->
                    succeed Key_Backslash

                ";" ->
                    succeed Key_Semicolon

                ":" ->
                    succeed Key_Semicolon

                "'" ->
                    succeed Key_Apostrophe

                "\"" ->
                    succeed Key_Apostrophe

                "," ->
                    succeed Key_Comma

                "<" ->
                    succeed Key_Comma

                "." ->
                    succeed Key_Period

                ">" ->
                    succeed Key_Period

                "/" ->
                    succeed Key_Slash

                "?" ->
                    succeed Key_Slash

                "1" ->
                    succeed Key_1

                "!" ->
                    succeed Key_1

                "2" ->
                    succeed Key_2

                "@" ->
                    succeed Key_2

                "3" ->
                    succeed Key_3

                "#" ->
                    succeed Key_3

                "4" ->
                    succeed Key_4

                "$" ->
                    succeed Key_4

                "5" ->
                    succeed Key_5

                "%" ->
                    succeed Key_5

                "6" ->
                    succeed Key_6

                "^" ->
                    succeed Key_6

                "7" ->
                    succeed Key_7

                "&" ->
                    succeed Key_7

                "8" ->
                    succeed Key_8

                "*" ->
                    succeed Key_8

                "9" ->
                    succeed Key_9

                "(" ->
                    succeed Key_9

                "0" ->
                    succeed Key_0

                ")" ->
                    succeed Key_0

                "q" ->
                    succeed Key_Q

                "Q" ->
                    succeed Key_Q

                "w" ->
                    succeed Key_W

                "W" ->
                    succeed Key_W

                "e" ->
                    succeed Key_E

                "E" ->
                    succeed Key_E

                "r" ->
                    succeed Key_R

                "R" ->
                    succeed Key_R

                "t" ->
                    succeed Key_T

                "T" ->
                    succeed Key_T

                "y" ->
                    succeed Key_Y

                "Y" ->
                    succeed Key_Y

                "u" ->
                    succeed Key_U

                "U" ->
                    succeed Key_U

                "i" ->
                    succeed Key_I

                "I" ->
                    succeed Key_I

                "o" ->
                    succeed Key_O

                "O" ->
                    succeed Key_O

                "p" ->
                    succeed Key_P

                "P" ->
                    succeed Key_P

                "a" ->
                    succeed Key_A

                "A" ->
                    succeed Key_A

                "s" ->
                    succeed Key_S

                "S" ->
                    succeed Key_S

                "d" ->
                    succeed Key_D

                "D" ->
                    succeed Key_D

                "f" ->
                    succeed Key_F

                "F" ->
                    succeed Key_F

                "g" ->
                    succeed Key_G

                "G" ->
                    succeed Key_G

                "h" ->
                    succeed Key_H

                "H" ->
                    succeed Key_H

                "j" ->
                    succeed Key_J

                "J" ->
                    succeed Key_J

                "k" ->
                    succeed Key_K

                "K" ->
                    succeed Key_K

                "l" ->
                    succeed Key_L

                "L" ->
                    succeed Key_L

                "z" ->
                    succeed Key_Z

                "Z" ->
                    succeed Key_Z

                "x" ->
                    succeed Key_X

                "X" ->
                    succeed Key_X

                "c" ->
                    succeed Key_C

                "C" ->
                    succeed Key_C

                "v" ->
                    succeed Key_V

                "V" ->
                    succeed Key_V

                "b" ->
                    succeed Key_B

                "B" ->
                    succeed Key_B

                "n" ->
                    succeed Key_N

                "N" ->
                    succeed Key_N

                "m" ->
                    succeed Key_M

                "M" ->
                    succeed Key_M

                _ ->
                    fail ("unrecognized keycode: " ++ str)

        keyDowns =
            Sub.map KeyDown (Browser.Events.onKeyDown keyDecoder)

        keyUps =
            Sub.map KeyUp (Browser.Events.onKeyUp keyDecoder)
    in
    Sub.batch [ keyDowns, keyUps ]
