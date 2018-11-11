module Extra exposing (doAfter, map, pure, setTimeout)

{-| Basics.Extra. Provides frequently used idiomatic helper.
-}

import Process
import Task exposing (Task)
import Time exposing (Posix)


{-| Just apply new Model, without any Cmd. Hence the name.
-}
pure : model -> ( model, Cmd msg, Bool )
pure m =
    ( m, Cmd.none, False )


map :
    (innerModel -> outerModel)
    -> (innerMsg -> outerMsg)
    -> ( innerModel, Cmd innerMsg, Bool )
    -> ( outerModel, Cmd outerMsg, Bool )
map modelTagger msgTagger ( m, cmd, shouldPersist ) =
    ( modelTagger m, Cmd.map msgTagger cmd, shouldPersist )


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
