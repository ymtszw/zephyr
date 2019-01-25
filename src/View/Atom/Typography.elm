module View.Atom.Typography exposing
    ( t, link, ntLink
    , sizeBase, sizeDetail, sizeHeadline, sizeTitle, sizeSection, sizeImpact
    , sansSerif, serif, monospace
    , italic, bold, underline
    , colorText, colorNote, colorLink, colorPrim, colorSucc, colorWarn, colorErr
    , newTab
    , styles, sizeBaseStyle, sansSerifStyle, monospaceStyle
    )

{-| Typography Atoms.

@docs t, link, ntLink
@docs sizeBase, sizeDetail, sizeHeadline, sizeTitle, sizeSection, sizeImpact
@docs sansSerif, serif, monospace
@docs italic, bold, underline
@docs colorText, colorNote, colorLink, colorPrim, colorSucc, colorWarn, colorErr
@docs newTab
@docs styles, sizeBaseStyle, sansSerifStyle, monospaceStyle

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import View.Atom.Layout as Layout
import View.Atom.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Style exposing (..)



-- HTML


{-| Just emits a given String to Html. Synonym of Html.text
-}
t : String -> Html msg
t =
    Html.text


{-| Creates a link. In this app we always target new tabs.

Mostly for inlines, but usable for blocks too.
Texts in links are automatically styled according to its upstream theme.

-}
link : List (Attribute msg) -> { url : String, children : List (Html msg) } -> Html msg
link attrs { url, children } =
    let
        linkAttrs =
            [ Attributes.href url
            , Attributes.rel "noopener noreferrer"
            ]
    in
    Html.a (linkAttrs ++ attrs) children


{-| Creates a link with `newTab`.
-}
ntLink : List (Attribute msg) -> { url : String, children : List (Html msg) } -> Html msg
ntLink attrs opts =
    link (newTab :: attrs) opts



-- CLASS


{-| Scale 0 (12px)

This equals to the global default, so you do not need this in an element
where global default is not overridden.

-}
sizeBase : Attribute msg
sizeBase =
    Attributes.class sizeBaseClass


{-| Scale -1 (9px)
-}
sizeDetail : Attribute msg
sizeDetail =
    Attributes.class sizeDetailClass


{-| Scale 1 (15px)
-}
sizeHeadline : Attribute msg
sizeHeadline =
    Attributes.class sizeHeadlineClass


{-| Scale 2 (18px)
-}
sizeTitle : Attribute msg
sizeTitle =
    Attributes.class sizeTitleClass


{-| Scale 4 (30px)
-}
sizeSection : Attribute msg
sizeSection =
    Attributes.class sizeSectionClass


{-| Scale 12 (174px). Used for background logo, and that's all.
-}
sizeImpact : Attribute msg
sizeImpact =
    Attributes.class sizeImpactClass


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


colorText : Attribute msg
colorText =
    Attributes.class colorTextClass


colorNote : Attribute msg
colorNote =
    Attributes.class colorNoteClass


colorLink : Attribute msg
colorLink =
    Attributes.class colorLinkClass


colorPrim : Attribute msg
colorPrim =
    Attributes.class colorPrimClass


colorSucc : Attribute msg
colorSucc =
    Attributes.class colorSuccClass


colorWarn : Attribute msg
colorWarn =
    Attributes.class colorWarnClass


colorErr : Attribute msg
colorErr =
    Attributes.class colorErrClass


{-| Used with `link`, sets "target='\_blank'".
-}
newTab : Attribute msg
newTab =
    Attributes.target "_blank"



-- STYLE


styles : List Style
styles =
    [ -- Font sizes
      sizeBaseStyle
    , s (c sizeDetailClass) [ ( "font-size", px (scale12 -1) ) ]
    , s (c sizeHeadlineClass) [ ( "font-size", px (scale12 1) ) ]
    , s (c sizeTitleClass) [ ( "font-size", px (scale12 2) ) ]
    , s (c sizeSectionClass) [ ( "font-size", px (scale12 4) ) ]
    , s (c sizeImpactClass) [ ( "font-size", px (scale12 12) ) ]
    , -- Font families
      sansSerifStyle
    , s (c serifClass) [ fontFamily [ "Georgia", "Palatino Linotype", "Times New Roman", "serif" ] ]
    , monospaceStyle
    , -- Font decorations
      s (c italicClass) [ ( "font-style", "italic" ) ]
    , s (c boldClass) [ ( "font-weight", "700" ) ]
    , s (c underlineClass) [ ( "text-decoration", "underline" ) ]
    ]
        ++ fontColorStyles
        ++ inlineCodeStyles
        ++ linkStyles


sizeBaseStyle : Style
sizeBaseStyle =
    s (c sizeBaseClass) [ ( "font-size", px (scale12 0) ) ]


sizeBaseClass : String
sizeBaseClass =
    "fsb"


sizeDetailClass : String
sizeDetailClass =
    "fsd"


sizeHeadlineClass : String
sizeHeadlineClass =
    "fsh"


sizeTitleClass : String
sizeTitleClass =
    "fst"


sizeSectionClass : String
sizeSectionClass =
    "fss"


sizeImpactClass : String
sizeImpactClass =
    "fsi"


sansSerifStyle : Style
sansSerifStyle =
    s (c sansSerifClass) [ fontFamily [ "Tahoma", "Verdana", "Arial", "Helvetica", "sans-serif" ] ]


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
    s (c monospaceClass) [ fontFamily [ "Consolas", "Lucida Console", "Monaco", "Courier New", "monospace" ] ]


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


fontColorStyles : List Style
fontColorStyles =
    [ s (c oneDarkClass) [ ( "color", cssRgba oneDarkTheme.text ) ] -- Default Font Color of the theme
    , f oneDarkClass colorTextClass oneDarkTheme.text
    , f oneDarkClass colorNoteClass oneDarkTheme.note
    , f oneDarkClass colorLinkClass oneDarkTheme.link
    , f oneDarkClass colorPrimClass oneDarkTheme.prim
    , f oneDarkClass colorSuccClass oneDarkTheme.succ
    , f oneDarkClass colorWarnClass oneDarkTheme.warn
    , f oneDarkClass colorErrClass oneDarkTheme.err
    , s (c aubergineClass) [ ( "color", cssRgba aubergineTheme.text ) ] -- Default Font Color of the theme
    , f aubergineClass colorTextClass aubergineTheme.text
    , f aubergineClass colorNoteClass aubergineTheme.note
    , f aubergineClass colorLinkClass aubergineTheme.link
    , f aubergineClass colorPrimClass aubergineTheme.prim
    , f aubergineClass colorSuccClass aubergineTheme.succ
    , f aubergineClass colorWarnClass aubergineTheme.warn
    , f aubergineClass colorErrClass aubergineTheme.err
    ]


f : String -> String -> Color -> Style
f themeClass modeClass color =
    scoped (c themeClass) (c modeClass) [ ( "color", cssRgba color ) ]


colorTextClass : String
colorTextClass =
    "fctext"


colorNoteClass : String
colorNoteClass =
    "fcnote"


colorLinkClass : String
colorLinkClass =
    "fclink"


colorPrimClass : String
colorPrimClass =
    "fcprim"


colorSuccClass : String
colorSuccClass =
    "fcsucc"


colorWarnClass : String
colorWarnClass =
    "fcwarn"


colorErrClass : String
colorErrClass =
    "fcerr"


inlineCodeStyles : List Style
inlineCodeStyles =
    [ s "code" [ ( "border-radius", "0.2em" ) ]
        |> inject monospaceStyle
        |> inject Layout.paddingInlineStyle
    , scoped (c oneDarkClass) "code" [ ( "color", cssRgba oneDarkTheme.err ), ( "background-color", cssRgba oneDarkTheme.bg ) ]
    , scoped (c aubergineClass) "code" [ ( "color", cssRgba aubergineTheme.err ), ( "background-color", cssRgba aubergineTheme.bg ) ]
    ]


linkStyles : List Style
linkStyles =
    [ s "a:link" [ ( "text-decoration", "none" ) ] -- Cancelling UA's default decorations
    , hov "a:link" [ ( "text-decoration", "underline" ) ]
    , scoped (c oneDarkClass) "a:link" [ ( "color", cssRgba oneDarkTheme.link ) ]
    , scoped (c oneDarkClass) "a:visited" [ ( "color", cssRgba oneDarkTheme.link ) ]
    , scoped (c aubergineClass) "a:link" [ ( "color", cssRgba aubergineTheme.link ) ]
    , scoped (c aubergineClass) "a:visited" [ ( "color", cssRgba aubergineTheme.link ) ]
    ]
