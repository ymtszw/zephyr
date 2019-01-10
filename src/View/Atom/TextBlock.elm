module View.Atom.TextBlock exposing
    ( forceBreak
    , styles, forceBreakStyle
    )

{-| Text Block Atoms.

@docs forceBreak
@docs styles, forceBreakStyle

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Atom.Typography as Typography
import View.Style exposing (..)


forceBreak : Attribute msg
forceBreak =
    class forceBreakClass


styles : List Style
styles =
    [ baseTextBlockStyle
    , forceBreakStyle
    , preStyle
    ]


baseTextBlockStyle : Style
baseTextBlockStyle =
    s "p,pre,h1,h2,h3,h4,h5,h6" [ ( "line-height", "1.3em" ) ]


forceBreakStyle : Style
forceBreakStyle =
    s (c forceBreakClass)
        [ ( "white-space", "pre-wrap" )
        , ( "word-break", "break-all" )
        ]


forceBreakClass : String
forceBreakClass =
    "fbr"


preStyle : Style
preStyle =
    derive "pre" Typography.monospaceStyle
        |> inject forceBreakStyle
