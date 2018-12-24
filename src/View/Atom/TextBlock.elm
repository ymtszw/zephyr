module View.Atom.TextBlock exposing (styles)

{-| Text Block Atoms.

@docs styles

-}

import View.Atom.Typography as Typography
import View.Style exposing (..)


styles : List Style
styles =
    [ baseTextBlockStyle
    , preStyle
    ]


baseTextBlockStyle : Style
baseTextBlockStyle =
    s "p,pre,h1,h2,h3,h4,h5,h6"
        [ ( "line-height", "1.3em" )
        , ( "white-space", "pre-wrap" )
        , ( "word-break", "break-all" )
        ]


preStyle : Style
preStyle =
    derive "pre" Typography.monospaceStyle
