module Extra exposing (ite, setTimeout)

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


{-| Convenient timer.

Sleep for a set amount of time, then fire event with current posix time.

-}
setTimeout : (Posix -> msg) -> Float -> Cmd msg
setTimeout timeoutMsg timeout =
    Process.sleep 5000
        |> Task.andThen (\() -> Time.now)
        |> Task.perform timeoutMsg
