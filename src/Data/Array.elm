module Data.Array exposing (all, moveFromTo, removeAt, splitAt, squeeze, update)

import Array exposing (Array, empty, slice)


all : (a -> Bool) -> Array a -> Bool
all check array =
    case Array.length array of
        0 ->
            True

        len ->
            allImpl check (len - 1) array


allImpl : (a -> Bool) -> Int -> Array a -> Bool
allImpl check index array =
    case Array.get index array of
        Just item ->
            -- Enfore TCO
            if check item then
                if index > 0 then
                    allImpl check (index - 1) array

                else
                    True

            else
                False

        Nothing ->
            -- Should not happen
            allImpl check index array


update : Int -> (a -> a) -> Array a -> Array a
update index fun array =
    case Array.get index array of
        Just e ->
            Array.set index (fun e) array

        Nothing ->
            array


{-| Split an Array at an index, such that the second half will start at the specified index.
-}
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


moveFromTo : Int -> Int -> Array a -> Array a
moveFromTo from to array =
    if from == to then
        array

    else
        case Array.get from array of
            Just moving ->
                if from < to then
                    moveToRight from to moving array

                else if 0 < to then
                    moveToLeft from to moving array

                else
                    -- In this case we always move target to 0
                    Array.append (Array.fromList [ moving ]) (removeAt from array)

            Nothing ->
                array


moveToRight : Int -> Int -> a -> Array a -> Array a
moveToRight from to moving array =
    let
        len =
            Array.length array

        leading =
            slice 0 from array

        leftShifted =
            slice (from + 1) (to + 1) array

        trailing =
            slice (to + 1) len array
    in
    Array.append leading (Array.append (Array.push moving leftShifted) trailing)


moveToLeft : Int -> Int -> a -> Array a -> Array a
moveToLeft from to moving array =
    let
        len =
            Array.length array

        leading =
            slice 0 to array

        rightShifted =
            slice to from array

        trailing =
            slice (from + 1) len array
    in
    Array.append (Array.push moving leading) (Array.append rightShifted trailing)


squeeze : Int -> a -> Array a -> Array a
squeeze index element array =
    let
        ( front, rear ) =
            splitAt index array
    in
    Array.append (Array.push element front) rear
