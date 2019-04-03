module StringExtra exposing
    ( splitAt, appendWithSpace, containsCaseIgnored
    , punctuateNumber
    , fromUrlShortened, toUrlUnsafe, truncateUrlLikeAt30
    )

{-| String (and Url) manipulations.

@docs splitAt, appendWithSpace, containsCaseIgnored
@docs punctuateNumber
@docs fromUrlShortened, toUrlUnsafe, truncateUrlLikeAt30

-}

import Url


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


fromUrlShortened : Url.Url -> String
fromUrlShortened url =
    let
        shortUrl =
            url.host ++ url.path
    in
    if String.endsWith "/" shortUrl then
        String.dropRight 1 shortUrl

    else
        shortUrl


truncateUrlLikeAt30 : String -> String
truncateUrlLikeAt30 urlLike =
    let
        dropScheme s =
            if String.startsWith "http://" s then
                String.dropLeft 7 s

            else if String.startsWith "https://" s then
                String.dropLeft 8 s

            else
                s

        ellipsis30 s =
            if String.length s > 30 then
                String.left 30 s ++ "..."

            else
                s
    in
    ellipsis30 (dropScheme urlLike)


toUrlUnsafe : String -> Url.Url
toUrlUnsafe raw =
    Url.fromString raw
        |> Maybe.withDefault
            { protocol = Url.Https
            , host = "example.com"
            , port_ = Nothing
            , path = ""
            , fragment = Nothing
            , query = Nothing
            }
