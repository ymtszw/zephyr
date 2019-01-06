module View.Atom.Border exposing
    ( solid, dotted, dashed
    , rect, round2, round5, gutter
    , color
    , styles
    )

{-| Border Atoms.

@docs solid, dotted, dashed
@docs rect, round2, round5, gutter
@docs color
@docs styles

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute)
import Html.Attributes as Attributes
import View.Style exposing (..)


solid : Attribute msg
solid =
    Attributes.class solidClass


dotted : Attribute msg
dotted =
    Attributes.class dottedClass


dashed : Attribute msg
dashed =
    Attributes.class dashedClass


rect : Attribute msg
rect =
    Attributes.class rectClass


round2 : Attribute msg
round2 =
    Attributes.class round2Class


round5 : Attribute msg
round5 =
    Attributes.class round5Class


{-| Since gutters are mostly used with user-defined colors,
it should be paired with `color` helper.
-}
gutter : Attribute msg
gutter =
    Attributes.class gutterClass


color : Color -> Attribute msg
color c =
    Attributes.style "border-color" (cssRgba c)


styles : List Style
styles =
    [ c solidClass [ ( "border-style", "solid" ) ]
    , c dottedClass [ ( "border-style", "dotted" ) ]
    , c dashedClass [ ( "border-style", "dashed" ) ]
    , c rectClass [ ( "border-width", "1px" ) ]
    , c round2Class [ ( "border-radius", "2px" ) ]
    , c round5Class [ ( "border-radius", "5px" ) ]
    , gutterStyle
    ]


solidClass : String
solidClass =
    "boso"


dottedClass : String
dottedClass =
    "bodo"


dashedClass : String
dashedClass =
    "boda"


rectClass : String
rectClass =
    "bore"


round2Class : String
round2Class =
    "boro2"


round5Class : String
round5Class =
    "boro5"


gutterStyle : Style
gutterStyle =
    c gutterClass
        [ ( "border-left-width", "5px" )
        , ( "border-left-style", "solid" )
        , ( "border-top-left-radius", "5px" )
        , ( "border-bottom-left-radius", "5px" )
        ]


gutterClass : String
gutterClass =
    "bogut"
