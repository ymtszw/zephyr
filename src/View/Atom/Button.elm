module View.Atom.Button exposing (styles)

{-| Button Atoms.

Color of buttons are decided by upstream themes and `Background`/`Typography` APIs.

@docs styles

-}

import Color exposing (Color, cssRgba)
import View.Atom.Layout as Layout
import View.Atom.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Style exposing (..)


styles : List Style
styles =
    [ oneDarkDefaultFaceStyle
    , defaultFaceStyle aubergineClass aubergineTheme.bd
    ]
        ++ standardStyles


standardStyles : List Style
standardStyles =
    [ s "button"
        [ ( "border-radius", "0.2em" )
        , ( "border-width", "0px" )
        , ( "cursor", "pointer" ) -- I deliberately want this. I am AGAINST the philosophy of "buttons do not need pointer."
        ]
        |> inject Layout.paddingInlineStyle
        |> inject oneDarkDefaultFaceStyle
    , s "button:hover" [ ( "opacity", "0.9" ) ]
    , s "button:disabled" [ ( "opacity", "0.7" ), ( "cursor", "default" ) ]
    ]


oneDarkDefaultFaceStyle : Style
oneDarkDefaultFaceStyle =
    -- .bd colors are used as defaults; these colors are not available as neither font nor BG colors
    defaultFaceStyle oneDarkClass oneDarkTheme.bd


defaultFaceStyle : String -> Color -> Style
defaultFaceStyle themeClass defaultColor =
    let
        selector =
            String.join ","
                [ "button." ++ themeClass
                , "." ++ themeClass ++ " button"
                ]
    in
    s selector [ ( "background-color", cssRgba defaultColor ) ]
