module View.Atom.Border exposing
    ( solid, dotted, dashed
    , rect, round2, round5
    , styles
    )

{-| Border Atoms.

@docs solid, dotted, dashed
@docs rect, round2, round5
@docs styles

-}

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


styles : List Style
styles =
    [ c solidClass [ ( "border-style", "solid" ) ]
    , c dottedClass [ ( "border-style", "dotted" ) ]
    , c dashedClass [ ( "border-style", "dashed" ) ]
    , c rectClass [ ( "border-width", "1px" ) ]
    , c round2Class [ ( "border-radius", "2px" ) ]
    , c round5Class [ ( "border-radius", "5px" ) ]
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
