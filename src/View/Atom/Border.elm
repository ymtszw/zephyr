module View.Atom.Border exposing
    ( solid, dotted, dashed
    , w1, noRound, round2, round5, elliptic, leftRound5, rightRound5, gutter
    , color
    , styles
    )

{-| Border Atoms.

@docs solid, dotted, dashed
@docs w1, noRound, round2, round5, elliptic, leftRound5, rightRound5, gutter
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


w1 : Attribute msg
w1 =
    Attributes.class w1Class


noRound : Attribute msg
noRound =
    Attributes.class noRoundClass


round2 : Attribute msg
round2 =
    Attributes.class round2Class


round5 : Attribute msg
round5 =
    Attributes.class round5Class


{-| 50% radius rounding. If the element is squared, it looks like a circle.
-}
elliptic : Attribute msg
elliptic =
    Attributes.class ellipticClass


leftRound5 : Attribute msg
leftRound5 =
    Attributes.class leftRound5Class


rightRound5 : Attribute msg
rightRound5 =
    Attributes.class rightRound5Class


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
    , c w1Class [ ( "border-width", "1px" ) ]
    , c noRoundClass [ ( "border-radius", "0px" ) ]
    , c round2Class [ ( "border-radius", "2px" ) ]
    , c round5Class [ ( "border-radius", "5px" ) ]
    , c ellipticClass [ ( "border-radius", "50%" ) ]
    , c leftRound5Class
        [ ( "border-top-left-radius", "5px" )
        , ( "border-top-right-radius", "0px" )
        , ( "border-bottom-right-radius", "0px" )
        , ( "border-bottom-left-radius", "5px" )
        ]
    , c rightRound5Class
        [ ( "border-top-left-radius", "0px" )
        , ( "border-top-right-radius", "5px" )
        , ( "border-bottom-right-radius", "5px" )
        , ( "border-bottom-left-radius", "0px" )
        ]
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


w1Class : String
w1Class =
    "bow1"


noRoundClass : String
noRoundClass =
    "bonor"


round2Class : String
round2Class =
    "boro2"


round5Class : String
round5Class =
    "boro5"


ellipticClass : String
ellipticClass =
    "boell"


leftRound5Class : String
leftRound5Class =
    "bolr5"


rightRound5Class : String
rightRound5Class =
    "borr5"


gutterStyle : Style
gutterStyle =
    c gutterClass
        [ ( "border-left-width", "5px" )
        , ( "border-left-style", "solid" )
        , ( "border-top-left-radius", "3px" )
        , ( "border-bottom-left-radius", "3px" )
        ]


gutterClass : String
gutterClass =
    "bogut"
