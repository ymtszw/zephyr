module Extra exposing (andDo, divMod, doAfter, ite, pure, setTimeout)

{-| Basics.Extra. Provides frequently used idiomatic helper.
-}

import Process
import Task exposing (Task)
import Time exposing (Posix)


{-| Replacement of (!) in 0.18.
-}
andDo : List (Cmd msg) -> model -> ( model, Cmd msg )
andDo cmds model =
    ( model, Cmd.batch cmds )


{-| Just apply new Model, without any Cmd. Hence the name.
-}
pure : model -> ( model, Cmd msg, Bool )
pure m =
    ( m, Cmd.none, False )


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


{-| Do a Task after a set amount of time.
-}
doAfter : Float -> (Result x ( Posix, a ) -> msg) -> Task x a -> Cmd msg
doAfter timeout toMsg taskOnTimeout =
    Task.map3
        (\() posix succ -> ( posix, succ ))
        (Process.sleep timeout)
        Time.now
        taskOnTimeout
        |> Task.attempt toMsg
