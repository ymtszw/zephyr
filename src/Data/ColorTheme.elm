module Data.ColorTheme exposing (ColorTheme, brightness, css, oneDark)

import Element exposing (Color, rgb, rgb255, toRgb)


type alias ColorTheme =
    { bg : Color
    , main : Color
    , sub : Color
    , bd : Color
    , text : Color
    , note : Color
    , link : Color
    , active : Color
    , succ : Color
    , warn : Color
    , err : Color
    }


oneDark : ColorTheme
oneDark =
    ColorTheme
        (rgb255 32 34 37)
        (rgb255 54 57 63)
        (rgb255 47 49 54)
        (rgb255 62 65 71)
        (rgb255 220 221 222)
        (rgb255 96 98 102)
        (rgb255 15 144 202)
        (rgb255 103 123 196)
        (rgb255 115 201 144)
        (rgb255 226 192 141)
        (rgb255 224 82 82)


{-| Shift brightness of a Color (RGB) by a power of 1.15, without altering alpha.

`brightness 1` on a Color of `{red = 100, green = 100, blue = 100}`
will yield `{red = 115, green = 115, blue = 115}`.

`brightness -1` on the same Color will yield `{red = 86.96, green = 86.96, blue = 86.96}`

-}
brightness : Float -> Color -> Color
brightness power color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgb (red * (1.15 ^ power)) (green * (1.15 ^ power)) (blue * (1.15 ^ power))


{-| Dump a Color to CSS-compatible representaiton
-}
css : Color -> String
css color =
    let
        { red, green, blue } =
            toRgb color
    in
    String.join ""
        [ "rgb("
        , String.fromFloat (255 * red)
        , ","
        , String.fromFloat (255 * green)
        , ","
        , String.fromFloat (255 * blue)
        , ")"
        ]
