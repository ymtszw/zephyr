module View.Atoms.TextBlock exposing
    ( forceBreak, breakWords, nowrap, selectAll, clip, ellipsis
    , styles, forceBreakStyle
    )

{-| Text Block Atoms.

@docs forceBreak, breakWords, nowrap, selectAll, clip, ellipsis
@docs styles, forceBreakStyle

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Atoms.Typography as Typography
import View.Style exposing (..)


{-| Wrap on literal line breaks, AND also break words.
-}
forceBreak : Attribute msg
forceBreak =
    class forceBreakClass


{-| Break words, but not wrap on literal line breaks.
-}
breakWords : Attribute msg
breakWords =
    class breakWordsClass


{-| Never wrap lines.
-}
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
    , s (c breakWordsClass) [ ( "word-break", "break-all" ) ]
    , s (c nowrapClass) [ ( "white-space", "nowrap" ) ]
    , s (c selectAllClass) [ ( "user-select", "all" ) ]
    , s (c clipClass) [ ( "overflow", "hidden" ) ]
    , s (c ellipsisClass) [ ( "overflow", "hidden" ), ( "text-overflow", "ellipsis" ) ]
    , preStyle
    ]
        ++ listStyles


baseTextBlockStyle : Style
baseTextBlockStyle =
    s "p,pre,blockquote,textarea,h1,h2,h3,h4,h5,h6,ul,ol" [ ( "line-height", "1.4em" ) ]


forceBreakStyle : Style
forceBreakStyle =
    s (c forceBreakClass)
        [ ( "white-space", "pre-wrap" )
        , ( "word-break", "break-all" )
        ]


breakWordsClass : String
breakWordsClass =
    "brw"


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


listStyles : List Style
listStyles =
    [ s "ul,ol"
        [ ( "list-style-position", "outside" )
        , ( "padding-left", px 20 ) -- Somewhat crude adjustment, compensating bullet points; effects may vary between browsers
        ]

    -- For unordered lists, just follow user-agent's default bullet point styles
    -- In Chrome it is disc > circle > square > square > square > ...
    , s "ol ol ol ol" [ ( "list-style-type", "decimal" ) ] -- 4 levels and thereafter, keep using decimal
    , s "ol ol ol" [ ( "list-style-type", "lower-latin" ) ]
    , s "ol ol" [ ( "list-style-type", "lower-roman" ) ]
    , s "ol" [ ( "list-style-type", "decimal" ) ]
    ]
