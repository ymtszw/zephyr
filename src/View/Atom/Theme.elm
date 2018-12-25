module View.Atom.Theme exposing
    ( Theme, aubergineTheme, oneDarkTheme
    , aubergine, oneDark
    , aubergineClass, oneDarkClass
    )

{-| Color Theme Atoms.

@docs Theme, aubergineTheme, oneDarkTheme
@docs aubergine, oneDark
@docs aubergineClass, oneDarkClass

-}

import Color exposing (Color, fromHexUnsafe)
import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Style exposing (Style)


type alias Theme =
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


oneDarkTheme : Theme
oneDarkTheme =
    { bg = fromHexUnsafe "#202225"
    , main = fromHexUnsafe "#35383e"
    , sub = fromHexUnsafe "#2f3136"
    , bd = fromHexUnsafe "#3e4147"
    , text = fromHexUnsafe "#dcddde"
    , note = fromHexUnsafe "#606266"
    , link = fromHexUnsafe "#0f90c9"
    , prim = fromHexUnsafe "#677bc4"
    , succ = fromHexUnsafe "#73c990"
    , warn = fromHexUnsafe "#e2c08d"
    , err = fromHexUnsafe "#e05252"
    }


aubergineTheme : Theme
aubergineTheme =
    { bg = fromHexUnsafe "#211820"
    , main = fromHexUnsafe "#4d394b"
    , sub = fromHexUnsafe "#3e313c"
    , bd = fromHexUnsafe "#6c676b"
    , text = fromHexUnsafe "#ffffff"
    , note = fromHexUnsafe "#909090"
    , link = fromHexUnsafe "#05b4b9"
    , prim = fromHexUnsafe "#4c9689"
    , succ = fromHexUnsafe "#008852"
    , warn = fromHexUnsafe "#ff9018"
    , err = fromHexUnsafe "#eb4d5c"
    }



-- CLASS
-- These classes are just tokens that indicate their contents must be colored using that theme.
-- Actual style definitions are found in Typography/Border/Background Atoms.


oneDark : Attribute msg
oneDark =
    class oneDarkClass


oneDarkClass : String
oneDarkClass =
    "oneDark"


aubergine : Attribute msg
aubergine =
    class aubergineClass


aubergineClass : String
aubergineClass =
    "aubergine"
