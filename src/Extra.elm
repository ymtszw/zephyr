module Extra exposing (doAfter, doT)

{-| Extensions for Platform.Cmd/Sub and Task.
-}

import Process
import Task exposing (Task)
import Time exposing (Posix)


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


doT : Task x a -> (a -> Task x b) -> Task x b
doT t1 toT2 =
    Task.andThen toT2 t1
