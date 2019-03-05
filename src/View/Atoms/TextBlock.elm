module View.Atoms.TextBlock exposing
    ( forceBreak, nowrap, selectAll, clip, ellipsis
    , styles, forceBreakStyle
    )

{-| Text Block Atoms.

@docs forceBreak, nowrap, selectAll, clip, ellipsis
@docs styles, forceBreakStyle

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Atoms.Typography as Typography
import View.Style exposing (..)


forceBreak : Attribute msg
forceBreak =
    class forceBreakClass


nowrap : Attribute msg
nowrap =
    class nowrapClass


selectAll : Attribute msg
selectAll =
    class selectAllClass


clip : Attribute msg
clip =
    class clipClass


ellipsis : Attribute msg
ellipsis =
    class ellipsisClass


styles : List Style
styles =
    [ baseTextBlockStyle
    , forceBreakStyle
    , s (c nowrapClass) [ ( "white-space", "nowrap" ) ]
    , s (c selectAllClass) [ ( "user-select", "all" ) ]
    , s (c clipClass) [ ( "overflow", "hidden" ) ]
    , s (c ellipsisClass) [ ( "overflow", "hidden" ), ( "text-overflow", "ellipsis" ) ]
    , preStyle
    ]


baseTextBlockStyle : Style
baseTextBlockStyle =
    s "p,pre,textarea,h1,h2,h3,h4,h5,h6" [ ( "line-height", "1.4em" ) ]


forceBreakStyle : Style
forceBreakStyle =
    s (c forceBreakClass)
        [ ( "white-space", "pre-wrap" )
        , ( "word-break", "break-all" )
        ]


forceBreakClass : String
forceBreakClass =
    "fbr"


nowrapClass : String
nowrapClass =
    "nwr"


preStyle : Style
preStyle =
    derive "pre" Typography.monospaceStyle
        |> inject forceBreakStyle


selectAllClass : String
selectAllClass =
    "slctall"


clipClass : String
clipClass =
    "clip"


ellipsisClass : String
ellipsisClass =
    "ellip"
