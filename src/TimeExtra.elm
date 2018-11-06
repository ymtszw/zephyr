module TimeExtra exposing (add, local, ms, posix, toNumMonth)

import Time exposing (Month(..), Posix, Zone)


posix : Int -> Posix
posix =
    Time.millisToPosix


ms : Posix -> Int
ms =
    Time.posixToMillis


add : Int -> Posix -> Posix
add millis =
    ms >> (+) millis >> posix


local : Zone -> Posix -> String
local z po =
    String.join ""
        [ Time.toYear z po |> String.fromInt
        , "/"
        , toNumMonth z po |> String.fromInt
        , "/"
        , Time.toDay z po |> String.fromInt
        , " "
        , Time.toHour z po |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , Time.toMinute z po |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , Time.toSecond z po |> String.fromInt |> String.padLeft 2 '0'
        ]


toNumMonth : Zone -> Posix -> Int
toNumMonth z po =
    case Time.toMonth z po of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
