module Data.ColorTheme exposing (ColorTheme, oneDark)

import Element exposing (Color, rgb255)


type alias ColorTheme =
    { bg : Color
    , main : Color
    , sub : Color
    , bd : Color
    , text : Color
    , note : Color
    , link : Color
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
        (rgb255 115 201 144)
        (rgb255 226 192 141)
        (rgb255 224 82 82)
