module TimeExtra exposing (add, ms, posix)

import Time exposing (Posix)


posix : Int -> Posix
posix =
    Time.millisToPosix


ms : Posix -> Int
ms =
    Time.posixToMillis


add : Int -> Posix -> Posix
add millis =
    ms >> (+) millis >> posix
