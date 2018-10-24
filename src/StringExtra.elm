module StringExtra exposing (appendWithSpace, splitAt)


splitAt : Int -> String -> List String
splitAt index raw =
    if index <= 0 || index == String.length raw || raw == "" then
        [ raw ]

    else
        [ String.slice 0 index raw, String.dropLeft index raw ]


appendWithSpace : String -> String -> String
appendWithSpace front back =
    if String.isEmpty front then
        back

    else if String.endsWith " " front then
        front ++ back

    else
        front ++ " " ++ back
