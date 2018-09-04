module Data.Array exposing (removeAt, splitAt)

import Array exposing (Array, empty, slice)


splitAt : Int -> Array a -> ( Array a, Array a )
splitAt index array =
    let
        len =
            Array.length array
    in
    case ( index >= 0, abs index < len ) of
        ( True, True ) ->
            ( slice 0 index array, slice index len array )

        ( True, False ) ->
            ( array, empty )

        ( False, True ) ->
            ( slice 0 (len + index) array, slice (len + index) len array )

        ( False, False ) ->
            ( empty, array )


removeAt : Int -> Array a -> Array a
removeAt index array =
    let
        ( front, rear ) =
            splitAt index array

        lenRear =
            Array.length rear
    in
    if lenRear == 0 then
        front

    else if negate index > lenRear then
        array

    else
        Array.append front (slice 1 lenRear rear)
