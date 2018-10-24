module StringExtra exposing (splitAt)


splitAt : Int -> String -> List String
splitAt index raw =
    if index <= 0 || index == String.length raw || raw == "" then
        [ raw ]

    else
        [ String.slice 0 index raw, String.dropLeft index raw ]
