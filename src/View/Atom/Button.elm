module View.Atom.Button exposing (styles, oneDarkDefaultStyle)

{-| Button Atoms.

Color of buttons are decided by upstream themes and `Background`/`Typography` APIs.

@docs styles, oneDarkDefaultStyle

-}

import Color exposing (Color, cssRgba)
import View.Atom.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Style exposing (..)


styles : List Style
styles =
    -- .bd colors are used as defaults; these colors are not available as neither font nor BG colors
    [ oneDarkDefaultStyle
    , defaultStyle aubergineClass aubergineTheme.bd
    ]


oneDarkDefaultStyle : Style
oneDarkDefaultStyle =
    defaultStyle oneDarkClass oneDarkTheme.bd


defaultStyle : String -> Color -> Style
defaultStyle themeClass defaultColor =
    let
        selector =
            String.join ","
                [ "button." ++ themeClass
                , "." ++ themeClass ++ " button"
                ]
    in
    s selector [ ( "background-color", cssRgba defaultColor ) ]
