module StringExtra exposing (appendWithSpace, containsCaseIgnored, punctuateNumber, splitAt)


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


punctuateNumber : Int -> String
punctuateNumber decimal =
    let
        reducer char ( accChars, accStr ) =
            case accChars of
                c1 :: c2 :: c3 :: _ ->
                    ( [ char ], String.fromList [ ',', c1, c2, c3 ] ++ accStr )

                _ ->
                    ( char :: accChars, accStr )

        ( rem, punctuated ) =
            decimal |> String.fromInt |> String.foldr reducer ( [], "" )
    in
    String.fromList rem ++ punctuated


containsCaseIgnored : String -> String -> Bool
containsCaseIgnored query str =
    String.contains (String.toLower query) (String.toLower str)
