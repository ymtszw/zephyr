module View.Atom.Typography exposing
    ( t
    , baseFontSize, detailFontSize, headlineFontSize, titleFontSize, impactFontSize
    , sansSerif, serif, monospace
    , styles, monospaceStyle
    )

{-| Typography Atoms.


## Html Renderers

@docs t


## Class Attributes

@docs baseFontSize, detailFontSize, headlineFontSize, titleFontSize, impactFontSize
@docs sansSerif, serif, monospace


## Style Entries

@docs styles, monospaceStyle

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


{-| Class that explicitly enforce `baseFontSize`.
-}
baseFontSize : Attribute msg
baseFontSize =
    Attributes.class baseFontSizeClass


detailFontSize : Attribute msg
detailFontSize =
    Attributes.class detailFontSizeClass


headlineFontSize : Attribute msg
headlineFontSize =
    Attributes.class headlineFontSizeClass


titleFontSize : Attribute msg
titleFontSize =
    Attributes.class titleFontSizeClass


impactFontSize : Attribute msg
impactFontSize =
    Attributes.class impactFontSizeClass


sansSerif : Attribute msg
sansSerif =
    Attributes.class sansSerifClass


serif : Attribute msg
serif =
    Attributes.class serifClass


monospace : Attribute msg
monospace =
    Attributes.class monospaceClass



-- STYLE


styles : List Style
styles =
    [ -- Font sizes
      baseFontSizeStyle
    , detailFontSizeStyle
    , headlineFontSizeStyle
    , titleFontSizeStyle
    , impactFontSizeStyle
    , -- Font families
      sansSerifStyle
    , serifStyle
    , monospaceStyle
    ]


{-| Scale 0 from 12px.

This equals to the global default, so you do not need this in an element
where global default is not overridden.

-}
baseFontSizeStyle : Style
baseFontSizeStyle =
    c baseFontSizeClass [ ( "font-size", px (scale12 0) ) ]


baseFontSizeClass : String
baseFontSizeClass =
    "bfs"


{-| Scale -1 (9px)
-}
detailFontSizeStyle : Style
detailFontSizeStyle =
    c detailFontSizeClass [ ( "font-size", px (scale12 -1) ) ]


detailFontSizeClass : String
detailFontSizeClass =
    "dfs"


{-| Scale 1 (15px)
-}
headlineFontSizeStyle : Style
headlineFontSizeStyle =
    c headlineFontSizeClass [ ( "font-size", px (scale12 1) ) ]


headlineFontSizeClass : String
headlineFontSizeClass =
    "hfs"


{-| Scale 2 (18px)
-}
titleFontSizeStyle : Style
titleFontSizeStyle =
    c titleFontSizeClass [ ( "font-size", px (scale12 2) ) ]


titleFontSizeClass : String
titleFontSizeClass =
    "tfs"


{-| Scale 12 (174px). Used for background logo, and that's all.
-}
impactFontSizeStyle : Style
impactFontSizeStyle =
    c impactFontSizeClass [ ( "font-size", px (scale12 12) ) ]


impactFontSizeClass : String
impactFontSizeClass =
    "ifs"


sansSerifStyle : Style
sansSerifStyle =
    c sansSerifClass
        [ Tuple.pair "font-family" <|
            String.join ","
                [ "Tahoma"
                , "Verdana"
                , "Arial"
                , "Helvetica"
                , "sans-serif"
                ]
        ]


sansSerifClass : String
sansSerifClass =
    "sansserif"


serifStyle : Style
serifStyle =
    c serifClass
        [ Tuple.pair "font-family" <|
            String.join ","
                [ "Georgia"
                , "Palatino Linotype"
                , "Times New Roman"
                , "sans-serif"
                ]
        ]


serifClass : String
serifClass =
    "serif"


monospaceStyle : Style
monospaceStyle =
    c monospaceClass
        [ Tuple.pair "font-family" <|
            String.join ","
                [ "Lucida Console"
                , "Monaco"
                , "Courier New"
                , "monospace"
                ]
        ]


monospaceClass : String
monospaceClass =
    "monospace"
