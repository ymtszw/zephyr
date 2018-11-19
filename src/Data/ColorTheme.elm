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
    , prim : Color
    , succ : Color
    , warn : Color
    , err : Color
    }


oneDark : ColorTheme
oneDark =
    { bg = rgb255 32 34 37
    , main = rgb255 54 57 63
    , sub = rgb255 47 49 54
    , bd = rgb255 62 65 71
    , text = rgb255 220 221 222
    , note = rgb255 96 98 102
    , link = rgb255 15 144 202
    , prim = rgb255 103 123 196
    , succ = rgb255 115 201 144
    , warn = rgb255 226 192 141
    , err = rgb255 224 82 82
    }
