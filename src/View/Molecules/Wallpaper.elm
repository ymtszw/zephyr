module View.Molecules.Wallpaper exposing (styles, zephyr)

import Color exposing (cssRgba)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.Atoms.Background as Background
import View.Atoms.Layout exposing (..)
import View.Atoms.Theme exposing (oneDark, oneDarkTheme)
import View.Atoms.Typography exposing (..)
import View.Style exposing (..)


zephyr : Html msg
zephyr =
    div
        [ class zephyrClass
        , oneDark
        , serif
        , impactful
        , flexColumn
        , flexCenter
        , Background.colorBg
        ]
        [ div [] [ t "Zephyr" ]
        ]


styles : List Style
styles =
    [ s (c zephyrClass)
        [ ( "position", "fixed" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "z-index", "-1" )
        , ( "width", "100vw" )
        , ( "height", "100vh" )
        , ( "justify-content", "center" )
        , ( "color", cssRgba oneDarkTheme.sub )
        ]
    ]


zephyrClass : String
zephyrClass =
    "wpz"
