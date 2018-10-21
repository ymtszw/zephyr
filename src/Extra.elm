module Extra exposing (divMod, ite, setTimeout)

{-| Basics.Extra. Provides frequently used idiomatic helper.
-}

import Process
import Task
import Time exposing (Posix)


{-| Oneline if-then-else.

Avoiding elm-format expansion.

-}
ite : Bool -> a -> a -> a
ite condition then_ else_ =
    if condition then
        then_

    else
        else_


{-| Integer division and modBy at once.

    divMod 2 5 == ( 2, 1 )

    divMod 25 100 == ( 4, 0 )

    divMod 4 -3 == ( 0, 1 )

-}
divMod : Int -> Int -> ( Int, Int )
divMod divisor dividend =
    ( dividend // divisor, modBy divisor dividend )


{-| Convenient timer.

Sleep for a set amount of time, then fire event with current posix time.

-}
setTimeout : (Posix -> msg) -> Float -> Cmd msg
setTimeout timeoutMsg timeout =
    Process.sleep timeout
        |> Task.andThen (\() -> Time.now)
        |> Task.perform timeoutMsg
