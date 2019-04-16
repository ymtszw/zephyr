module Data.Producer.Discord.Cdn exposing (makeUrl)


makeUrl : Maybe Int -> String -> String
makeUrl sizeMaybe path =
    let
        sizeQuery =
            case sizeMaybe of
                Just size ->
                    "?size=" ++ String.fromInt (imageQuerySize size)

                Nothing ->
                    ""
    in
    "https://cdn.discordapp.com" ++ path ++ sizeQuery


imageQuerySize : Int -> Int
imageQuerySize size =
    if size > 512 then
        1024

    else if size > 256 then
        512

    else if size > 128 then
        256

    else if size > 64 then
        128

    else if size > 32 then
        64

    else if size > 16 then
        32

    else
        16
