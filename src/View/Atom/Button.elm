module View.Atom.Button exposing
    ( prim, succ, warn, err
    , styles
    )

{-| Button Atoms.

Color of buttons are decided by upstream themes.

@docs prim, succ, warn, err
@docs styles

-}

import Color exposing (Color, brightness, cssRgba)
import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Atom.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Atom.Typography exposing (colorText)
import View.Style exposing (..)


prim : Attribute msg
prim =
    class primClass


succ : Attribute msg
succ =
    class succClass


warn : Attribute msg
warn =
    class warnClass


err : Attribute msg
err =
    class errClass


styles : List Style
styles =
    List.concat
        [ btn oneDarkClass primClass oneDarkTheme.prim oneDarkTheme.text
        , btn oneDarkClass succClass oneDarkTheme.succ oneDarkTheme.text
        , btn oneDarkClass warnClass oneDarkTheme.warn oneDarkTheme.text
        , btn oneDarkClass errClass oneDarkTheme.err oneDarkTheme.text
        , btn aubergineClass primClass aubergineTheme.prim aubergineTheme.text
        , btn aubergineClass succClass aubergineTheme.succ aubergineTheme.text
        , btn aubergineClass warnClass aubergineTheme.warn aubergineTheme.text
        , btn aubergineClass errClass aubergineTheme.err aubergineTheme.text
        ]


btn : String -> String -> Color -> Color -> List Style
btn themeClass modeClass bgColor fontColor =
    let
        selectorBase =
            [ "." ++ themeClass ++ "." ++ modeClass -- Same element
            , "." ++ themeClass ++ " ." ++ modeClass -- Descendants
            ]
    in
    [ s (String.join "," selectorBase)
        [ ( "background-color", cssRgba bgColor )
        , ( "border-radius", "5px" )
        , ( "border-width", "0px" )
        , ( "color", cssRgba fontColor )
        , ( "cursor", "pointer" )
        ]
    , s (String.join "," (List.map (\c -> c ++ ":hover") selectorBase))
        [ ( "background-color", cssRgba (brightness -1 bgColor) ) ]
    ]


primClass : String
primClass =
    "primbtn"


succClass : String
succClass =
    "succbtn"


warnClass : String
warnClass =
    "warnbtn"


errClass : String
errClass =
    "errbtn"
