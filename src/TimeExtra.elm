module TimeExtra exposing (add, local, ms, po, toNumMonth)

import Time exposing (Month(..), Posix, Zone)


po : Int -> Posix
po =
    Time.millisToPosix


ms : Posix -> Int
ms =
    Time.posixToMillis


add : Int -> Posix -> Posix
add millis =
    ms >> (+) millis >> po


local : Zone -> Posix -> String
local z posix =
    String.join ""
        [ Time.toYear z posix |> String.fromInt
        , "/"
        , toNumMonth z posix |> String.fromInt
        , "/"
        , Time.toDay z posix |> String.fromInt
        , " "
        , Time.toHour z posix |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , Time.toMinute z posix |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , Time.toSecond z posix |> String.fromInt |> String.padLeft 2 '0'
        ]


toNumMonth : Zone -> Posix -> Int
toNumMonth z posix =
    case Time.toMonth z posix of
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
