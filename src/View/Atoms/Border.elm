module View.Atoms.Border exposing
    ( solid, dotted, dashed
    , w1, y1, bot1, noRound, round2, round5, elliptic, topRound5, bottomRound5, leftRound5, rightRound5, gutter
    , color, colorBd, colorBg, colorSub, colorNote
    , styles, round2Style, round5Style
    )

{-| Border Atoms.

@docs solid, dotted, dashed
@docs w1, y1, bot1, noRound, round2, round5, elliptic, topRound5, bottomRound5, leftRound5, rightRound5, gutter
@docs color, colorBd, colorBg, colorSub, colorNote
@docs styles, round2Style, round5Style

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute)
import Html.Attributes as Attributes
import View.Atoms.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
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


y1 : Attribute msg
y1 =
    Attributes.class y1Class


bot1 : Attribute msg
bot1 =
    Attributes.class bot1Class


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


topRound5 : Attribute msg
topRound5 =
    Attributes.class topRound5Class


bottomRound5 : Attribute msg
bottomRound5 =
    Attributes.class bottomRound5Class


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
color color_ =
    Attributes.style "border-color" (cssRgba color_)


colorBd : Attribute msg
colorBd =
    Attributes.class colorBdClass


colorBg : Attribute msg
colorBg =
    Attributes.class colorBgClass


colorSub : Attribute msg
colorSub =
    Attributes.class colorSubClass


colorNote : Attribute msg
colorNote =
    Attributes.class colorNoteClass


styles : List Style
styles =
    [ s (c oneDarkClass ++ " *") [ ( "border-color", cssRgba oneDarkTheme.bd ) ] -- oneDark default
    , s (c aubergineClass ++ " *") [ ( "border-color", cssRgba aubergineTheme.bd ) ] -- aubergine default
    , s (c solidClass) [ ( "border-style", "solid" ) ]
    , s (c dottedClass) [ ( "border-style", "dotted" ) ]
    , s (c dashedClass) [ ( "border-style", "dashed" ) ]
    , s (c w1Class) [ ( "border-width", "1px" ) ]
    , s (c y1Class) [ ( "border-top-width", "1px" ), ( "border-bottom-width", "1px" ) ]
    , s (c bot1Class) [ ( "border-bottom-width", "1px" ) ]
    , s (c noRoundClass) [ ( "border-radius", "0px" ) ]
    , round2Style
    , round5Style
    , s (c ellipticClass) [ ( "border-radius", "50%" ) ]
    , s (c topRound5Class)
        [ ( "border-top-left-radius", "5px" )
        , ( "border-top-right-radius", "5px" )
        , ( "border-bottom-right-radius", "0px" )
        , ( "border-bottom-left-radius", "0px" )
        ]
    , s (c bottomRound5Class)
        [ ( "border-top-left-radius", "0px" )
        , ( "border-top-right-radius", "0px" )
        , ( "border-bottom-right-radius", "5px" )
        , ( "border-bottom-left-radius", "5px" )
        ]
    , s (c leftRound5Class)
        [ ( "border-top-left-radius", "5px" )
        , ( "border-top-right-radius", "0px" )
        , ( "border-bottom-right-radius", "0px" )
        , ( "border-bottom-left-radius", "5px" )
        ]
    , s (c rightRound5Class)
        [ ( "border-top-left-radius", "0px" )
        , ( "border-top-right-radius", "5px" )
        , ( "border-bottom-right-radius", "5px" )
        , ( "border-bottom-left-radius", "0px" )
        ]
    , gutterStyle
    , bdc oneDarkClass colorBdClass oneDarkTheme.bd
    , bdc oneDarkClass colorBgClass oneDarkTheme.bg
    , bdc oneDarkClass colorSubClass oneDarkTheme.sub
    , bdc oneDarkClass colorNoteClass oneDarkTheme.note
    , bdc aubergineClass colorBdClass aubergineTheme.bd
    , bdc aubergineClass colorBgClass aubergineTheme.bg
    , bdc aubergineClass colorSubClass aubergineTheme.sub
    , bdc aubergineClass colorNoteClass aubergineTheme.note
    , s "hr" [ ( "border-top-width", "1px" ), ( "border-top-style", "solid" ) ]
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


y1Class : String
y1Class =
    "boy1"


bot1Class : String
bot1Class =
    "bob1"


noRoundClass : String
noRoundClass =
    "bonor"


round2Style : Style
round2Style =
    s (c round2Class) [ ( "border-radius", "2px" ) ]


round2Class : String
round2Class =
    "boro2"


round5Style : Style
round5Style =
    s (c round5Class) [ ( "border-radius", "5px" ) ]


round5Class : String
round5Class =
    "boro5"


ellipticClass : String
ellipticClass =
    "boell"


topRound5Class : String
topRound5Class =
    "botr5"


bottomRound5Class : String
bottomRound5Class =
    "bobr5"


leftRound5Class : String
leftRound5Class =
    "bolr5"


rightRound5Class : String
rightRound5Class =
    "borr5"


gutterStyle : Style
gutterStyle =
    s (c gutterClass)
        [ ( "border-left-width", "3px" )
        , ( "border-left-style", "solid" )
        , ( "border-top-left-radius", "3px" )
        , ( "border-bottom-left-radius", "3px" )
        ]


gutterClass : String
gutterClass =
    "bogut"


bdc : String -> String -> Color -> Style
bdc themeClass modeClass color_ =
    scoped (c themeClass) (c modeClass) [ ( "border-color", cssRgba color_ ) ]


colorBdClass : String
colorBdClass =
    "bdcbd"


colorBgClass : String
colorBgClass =
    "bdcbg"


colorSubClass : String
colorSubClass =
    "bdcsub"


colorNoteClass : String
colorNoteClass =
    "bdcnote"
