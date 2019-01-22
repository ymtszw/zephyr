module View.Atom.TextBlock exposing
    ( forceBreak, selectAll
    , styles, forceBreakStyle
    )

{-| Text Block Atoms.

@docs forceBreak, selectAll
@docs styles, forceBreakStyle

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Atom.Typography as Typography
import View.Style exposing (..)


forceBreak : Attribute msg
forceBreak =
    class forceBreakClass


selectAll : Attribute msg
selectAll =
    class selectAllClass


styles : List Style
styles =
    [ baseTextBlockStyle
    , forceBreakStyle
    , s (c selectAllClass) [ ( "user-select", "all" ) ]
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


selectAllClass : String
selectAllClass =
    "slctall"
