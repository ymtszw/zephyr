module View.Atom.Background exposing
    ( colorBg, colorMain, colorSub, colorText, colorNote, colorPrim, colorSucc, colorWarn, colorErr, transparent
    , hovSub
    , styles, oneDarkMainStyle, aubergineMainStyle
    )

{-| Background Atoms.

@docs colorBg, colorMain, colorSub, colorText, colorNote, colorPrim, colorSucc, colorWarn, colorErr, transparent
@docs hovSub
@docs styles, oneDarkMainStyle, aubergineMainStyle

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Atom.Theme exposing (..)
import View.Style exposing (..)



-- Class


colorBg : Attribute msg
colorBg =
    class colorBgClass


colorMain : Attribute msg
colorMain =
    class colorMainClass


colorSub : Attribute msg
colorSub =
    class colorSubClass


colorText : Attribute msg
colorText =
    class colorTextClass


colorNote : Attribute msg
colorNote =
    class colorNoteClass


colorPrim : Attribute msg
colorPrim =
    class colorPrimClass


colorSucc : Attribute msg
colorSucc =
    class colorSuccClass


colorWarn : Attribute msg
colorWarn =
    class colorWarnClass


colorErr : Attribute msg
colorErr =
    class colorErrClass


transparent : Attribute msg
transparent =
    class transparentClass


hovSub : Attribute msg
hovSub =
    class hovSubClass



-- Styles


styles : List Style
styles =
    -- XXX Order matters!
    [ oneDarkMainStyle
    , bg oneDarkClass colorBgClass oneDarkTheme.bg
    , bg oneDarkClass colorMainClass oneDarkTheme.main
    , bg oneDarkClass colorSubClass oneDarkTheme.sub
    , bg oneDarkClass colorTextClass oneDarkTheme.text
    , bg oneDarkClass colorNoteClass oneDarkTheme.note
    , bg oneDarkClass colorPrimClass oneDarkTheme.prim
    , bg oneDarkClass colorSuccClass oneDarkTheme.succ
    , bg oneDarkClass colorWarnClass oneDarkTheme.warn
    , bg oneDarkClass colorErrClass oneDarkTheme.err
    , hovBg oneDarkClass hovSubClass oneDarkTheme.sub
    , scoped (c oneDarkClass) (c transparentClass) [ ( "background-color", "transparent" ) ]
    , aubergineMainStyle
    , bg aubergineClass colorBgClass aubergineTheme.bg
    , bg aubergineClass colorMainClass aubergineTheme.main
    , bg aubergineClass colorSubClass aubergineTheme.sub
    , bg aubergineClass colorTextClass aubergineTheme.text
    , bg aubergineClass colorNoteClass aubergineTheme.note
    , bg aubergineClass colorPrimClass aubergineTheme.prim
    , bg aubergineClass colorSuccClass aubergineTheme.succ
    , bg aubergineClass colorWarnClass aubergineTheme.warn
    , bg aubergineClass colorErrClass aubergineTheme.err
    , hovBg aubergineClass hovSubClass aubergineTheme.sub
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


colorTextClass : String
colorTextClass =
    "bgctext"


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


hovBg : String -> String -> Color -> Style
hovBg themeClass modeClass color =
    scoped (c themeClass) (hov (c modeClass)) [ ( "background-color", cssRgba color ) ]


hovSubClass : String
hovSubClass =
    "bghovsub"
