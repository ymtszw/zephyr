module View.Atom.Background exposing
    ( colorBg, colorMain, colorSub, colorNote, colorPrim, colorSucc, colorWarn, colorErr, transparent
    , styles, oneDarkMainStyle, aubergineMainStyle
    )

{-| Background Atoms.

@docs colorBg, colorMain, colorSub, colorNote, colorPrim, colorSucc, colorWarn, colorErr, transparent
@docs styles, oneDarkMainStyle, aubergineMainStyle

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute)
import Html.Attributes as Attributes
import View.Atom.Theme exposing (..)
import View.Style exposing (..)



-- Class


colorBg : Attribute msg
colorBg =
    Attributes.class colorBgClass


colorMain : Attribute msg
colorMain =
    Attributes.class colorMainClass


colorSub : Attribute msg
colorSub =
    Attributes.class colorSubClass


colorNote : Attribute msg
colorNote =
    Attributes.class colorNoteClass


colorPrim : Attribute msg
colorPrim =
    Attributes.class colorPrimClass


colorSucc : Attribute msg
colorSucc =
    Attributes.class colorSuccClass


colorWarn : Attribute msg
colorWarn =
    Attributes.class colorWarnClass


colorErr : Attribute msg
colorErr =
    Attributes.class colorErrClass


transparent : Attribute msg
transparent =
    Attributes.class transparentClass



-- Styles


styles : List Style
styles =
    -- XXX Order matters!
    [ oneDarkMainStyle
    , bg oneDarkClass colorBgClass oneDarkTheme.bg
    , bg oneDarkClass colorMainClass oneDarkTheme.main
    , bg oneDarkClass colorSubClass oneDarkTheme.sub
    , bg oneDarkClass colorNoteClass oneDarkTheme.note
    , bg oneDarkClass colorPrimClass oneDarkTheme.prim
    , bg oneDarkClass colorSuccClass oneDarkTheme.succ
    , bg oneDarkClass colorWarnClass oneDarkTheme.warn
    , bg oneDarkClass colorErrClass oneDarkTheme.err
    , scoped (c oneDarkClass) (c transparentClass) [ ( "background-color", "transparent" ) ]
    , aubergineMainStyle
    , bg aubergineClass colorBgClass aubergineTheme.bg
    , bg aubergineClass colorMainClass aubergineTheme.main
    , bg aubergineClass colorSubClass aubergineTheme.sub
    , bg aubergineClass colorNoteClass aubergineTheme.note
    , bg aubergineClass colorPrimClass aubergineTheme.prim
    , bg aubergineClass colorSuccClass aubergineTheme.succ
    , bg aubergineClass colorWarnClass aubergineTheme.warn
    , bg aubergineClass colorErrClass aubergineTheme.err
    , scoped (c aubergineClass) (c transparentClass) [ ( "background-color", "transparent" ) ]
    ]


oneDarkMainStyle : Style
oneDarkMainStyle =
    s (c oneDarkClass) [ ( "background-color", cssRgba oneDarkTheme.main ) ]


aubergineMainStyle : Style
aubergineMainStyle =
    s (c aubergineClass) [ ( "background-color", cssRgba aubergineTheme.main ) ]


bg : String -> String -> Color -> Style
bg themeClass modeClass color =
    scoped (c themeClass) (c modeClass) [ ( "background-color", cssRgba color ) ]


colorBgClass : String
colorBgClass =
    "bgcbg"


colorMainClass : String
colorMainClass =
    "bgcmain"


colorSubClass : String
colorSubClass =
    "bgcsub"


colorNoteClass : String
colorNoteClass =
    "bgcnote"


colorPrimClass : String
colorPrimClass =
    "bgcprim"


colorSuccClass : String
colorSuccClass =
    "bgcsucc"


colorWarnClass : String
colorWarnClass =
    "bgcwarn"


colorErrClass : String
colorErrClass =
    "bgcerr"


transparentClass : String
transparentClass =
    "bgtrans"
