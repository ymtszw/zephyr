module View.Atom.Typography exposing
    ( t
    , baseFontSize, detailFontSize, headlineFontSize, titleFontSize, sectionFontSize, impactFontSize
    , sansSerif, serif, monospace
    , italic, bold, underline
    , styles, baseFontSizeStyle, sansSerifStyle, monospaceStyle
    )

{-| Typography Atoms.

@docs t
@docs baseFontSize, detailFontSize, headlineFontSize, titleFontSize, sectionFontSize, impactFontSize
@docs sansSerif, serif, monospace
@docs italic, bold, underline
@docs styles, baseFontSizeStyle, sansSerifStyle, monospaceStyle

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import View.Style exposing (..)



-- HTML


{-| Just emit a given String to Html. Synonym of Html.text
-}
t : String -> Html msg
t =
    Html.text



-- CLASS
-- Font sizes


{-| Scale 0 (12px)

This equals to the global default, so you do not need this in an element
where global default is not overridden.

-}
baseFontSize : Attribute msg
baseFontSize =
    Attributes.class baseFontSizeClass


{-| Scale -1 (9px)
-}
detailFontSize : Attribute msg
detailFontSize =
    Attributes.class detailFontSizeClass


{-| Scale 1 (15px)
-}
headlineFontSize : Attribute msg
headlineFontSize =
    Attributes.class headlineFontSizeClass


{-| Scale 2 (18px)
-}
titleFontSize : Attribute msg
titleFontSize =
    Attributes.class titleFontSizeClass


{-| Scale 4 (30px)
-}
sectionFontSize : Attribute msg
sectionFontSize =
    Attributes.class sectionFontSizeClass


{-| Scale 12 (174px). Used for background logo, and that's all.
-}
impactFontSize : Attribute msg
impactFontSize =
    Attributes.class impactFontSizeClass



-- Font families


{-| Sans-serif fonts. Used as the global default.
-}
sansSerif : Attribute msg
sansSerif =
    Attributes.class sansSerifClass


serif : Attribute msg
serif =
    Attributes.class serifClass


monospace : Attribute msg
monospace =
    Attributes.class monospaceClass


italic : Attribute msg
italic =
    Attributes.class italicClass


bold : Attribute msg
bold =
    Attributes.class boldClass


underline : Attribute msg
underline =
    Attributes.class underlineClass



-- STYLE


styles : List Style
styles =
    [ -- Font sizes
      baseFontSizeStyle
    , c detailFontSizeClass [ ( "font-size", px (scale12 -1) ) ]
    , c headlineFontSizeClass [ ( "font-size", px (scale12 1) ) ]
    , c titleFontSizeClass [ ( "font-size", px (scale12 2) ) ]
    , c sectionFontSizeClass [ ( "font-size", px (scale12 4) ) ]
    , c impactFontSizeClass [ ( "font-size", px (scale12 12) ) ]
    , -- Font families
      sansSerifStyle
    , c serifClass [ fontFamily [ "Georgia", "Palatino Linotype", "Times New Roman", "sans-serif" ] ]
    , monospaceStyle
    , -- Font decorations
      c italicClass [ ( "font-style", "italic" ) ]
    , c boldClass [ ( "font-weight", "700" ) ]
    , c underlineClass [ ( "text-decoration", "underline" ) ]
    ]


baseFontSizeStyle : Style
baseFontSizeStyle =
    c baseFontSizeClass [ ( "font-size", px (scale12 0) ) ]


baseFontSizeClass : String
baseFontSizeClass =
    "bfs"


detailFontSizeClass : String
detailFontSizeClass =
    "dfs"


headlineFontSizeClass : String
headlineFontSizeClass =
    "hfs"


titleFontSizeClass : String
titleFontSizeClass =
    "tfs"


sectionFontSizeClass : String
sectionFontSizeClass =
    "sfs"


impactFontSizeClass : String
impactFontSizeClass =
    "ifs"


sansSerifStyle : Style
sansSerifStyle =
    c sansSerifClass [ fontFamily [ "Tahoma", "Verdana", "Arial", "Helvetica", "sans-serif" ] ]


sansSerifClass : String
sansSerifClass =
    "sansserif"


fontFamily : List String -> ( String, String )
fontFamily fonts =
    ( "font-family", String.join "," fonts )


serifClass : String
serifClass =
    "serif"


monospaceStyle : Style
monospaceStyle =
    c monospaceClass [ fontFamily [ "Lucida Console", "Monaco", "Courier New", "monospace" ] ]


monospaceClass : String
monospaceClass =
    "monospace"


italicClass : String
italicClass =
    "ita"


boldClass : String
boldClass =
    "bold"


underlineClass : String
underlineClass =
    "u"
