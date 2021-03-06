module View.Atoms.Typography exposing
    ( t, link, ntLink
    , regular, minuscule, prominent, xProminent, xxProminent, xxxProminent, impactful
    , regularSize, minusculeSize, prominentSize, xProminentSize, xxProminentSize, xxxProminentSize, impactfulSize
    , sansSerif, serif, monospace
    , italic, bold, underline
    , colorText, colorNote, colorLink, colorPrim, colorSucc, colorWarn, colorErr
    , newTab
    , styles, regularStyle, sansSerifStyle, monospaceStyle
    , phColorNote, phColorText
    )

{-| Typography Atoms.

@docs t, link, ntLink
@docs regular, minuscule, prominent, xProminent, xxProminent, xxxProminent, impactful
@docs regularSize, minusculeSize, prominentSize, xProminentSize, xxProminentSize, xxxProminentSize, impactfulSize
@docs sansSerif, serif, monospace
@docs italic, bold, underline
@docs colorText, colorNote, colorLink, colorPrim, colorSucc, colorWarn, colorErr
@docs newTab
@docs styles, regularStyle, sansSerifStyle, monospaceStyle

-}

import Color exposing (toCssString)
import ColorExtra exposing (setAlpha)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import View.Atoms.Layout as Layout
import View.Atoms.Theme exposing (aubergineClass, aubergineTheme, oneDarkClass, oneDarkTheme)
import View.Style exposing (..)



-- HTML


{-| Just emits a given String to Html. Synonym of Html.text
-}
t : String -> Html msg
t =
    Html.text


{-| Creates a link.

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
regular : Attribute msg
regular =
    Attributes.class regularClass


{-| Scale 0 (12)
-}
regularSize : Int
regularSize =
    scale12 0


{-| Scale -1 (9px)
-}
minuscule : Attribute msg
minuscule =
    Attributes.class minusculeClass


{-| Scale -1 (9)
-}
minusculeSize : Int
minusculeSize =
    scale12 -1


{-| Scale 1 (15px)
-}
prominent : Attribute msg
prominent =
    Attributes.class prominentClass


{-| Scale 1 (15)
-}
prominentSize : Int
prominentSize =
    scale12 1


{-| Scale 2 (18px)
-}
xProminent : Attribute msg
xProminent =
    Attributes.class xProminentClass


{-| Scale 2 (18)
-}
xProminentSize : Int
xProminentSize =
    scale12 2


{-| Scale 4 (30px)
-}
xxProminent : Attribute msg
xxProminent =
    Attributes.class xxProminentClass


{-| Scale 4 (30)
-}
xxProminentSize : Int
xxProminentSize =
    scale12 4


{-| Scale 8 (71px)
-}
xxxProminent : Attribute msg
xxxProminent =
    Attributes.class xxxProminentClass


{-| Scale 8 (71)
-}
xxxProminentSize : Int
xxxProminentSize =
    scale12 8


{-| Scale 12 (174px). Used for background logo, and that's all.
-}
impactful : Attribute msg
impactful =
    Attributes.class impactfulClass


{-| Scale 12 (174)
-}
impactfulSize : Int
impactfulSize =
    scale12 12


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


phColorText : Attribute msg
phColorText =
    Attributes.class phColorTextClass


phColorNote : Attribute msg
phColorNote =
    Attributes.class phColorNoteClass


{-| Used with `link`, sets "target='\_blank'".
-}
newTab : Attribute msg
newTab =
    Attributes.target "_blank"



-- STYLE


styles : List Style
styles =
    [ -- Font sizes
      regularStyle
    , s (c minusculeClass) [ ( "font-size", px minusculeSize ) ]
    , s (c prominentClass) [ ( "font-size", px prominentSize ) ]
    , s (c xProminentClass) [ ( "font-size", px xProminentSize ) ]
    , s (c xxProminentClass) [ ( "font-size", px xxProminentSize ) ]
    , s (c xxxProminentClass) [ ( "font-size", px xxxProminentSize ) ]
    , s (c impactfulClass) [ ( "font-size", px impactfulSize ) ]
    , -- Font families
      sansSerifStyle
    , s (c serifClass) [ fontFamily [ "Georgia", "Palatino Linotype", "Times New Roman", "serif" ] ]
    , monospaceStyle
    , -- Font decorations
      s (c italicClass) [ ( "font-style", "italic" ) ]
    , s "em" [ ( "font-style", "italic" ) ]
    , s (c boldClass) [ ( "font-weight", "700" ) ]
    , s "strong" [ ( "font-weight", "700" ) ]
    , s (c underlineClass) [ ( "text-decoration", "underline" ) ]
    , s "strong > strong" [ ( "text-decoration", "underline" ) ] -- Nested strong should semantically mean "stronger", applying underline to visually express that
    ]
        ++ fontColorStyles
        ++ inlineCodeStyles
        ++ linkStyles


regularStyle : Style
regularStyle =
    s (c regularClass) [ ( "font-size", px regularSize ) ]


regularClass : String
regularClass =
    "fsr"


minusculeClass : String
minusculeClass =
    "fsm"


prominentClass : String
prominentClass =
    "fsp"


xProminentClass : String
xProminentClass =
    "fsxp"


xxProminentClass : String
xxProminentClass =
    "fsxxp"


xxxProminentClass : String
xxxProminentClass =
    "fsxxxp"


impactfulClass : String
impactfulClass =
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
    let
        fc themeClass modeClass color =
            scoped (c themeClass) (c modeClass) [ ( "color", toCssString color ) ]

        ph themeClass modeClass color =
            scoped (c themeClass) (c modeClass ++ "::placeholder") [ ( "color", toCssString color ) ]
    in
    [ s (c oneDarkClass) [ ( "color", toCssString oneDarkTheme.text ) ] -- Default Font Color of the theme
    , fc oneDarkClass colorTextClass oneDarkTheme.text
    , fc oneDarkClass colorNoteClass oneDarkTheme.note
    , fc oneDarkClass colorLinkClass oneDarkTheme.link
    , fc oneDarkClass colorPrimClass oneDarkTheme.prim
    , fc oneDarkClass colorSuccClass oneDarkTheme.succ
    , fc oneDarkClass colorWarnClass oneDarkTheme.warn
    , fc oneDarkClass colorErrClass oneDarkTheme.err
    , ph oneDarkClass phColorTextClass (setAlpha 0.3 oneDarkTheme.text)
    , ph oneDarkClass phColorNoteClass (setAlpha 0.6 oneDarkTheme.note)
    , s (c aubergineClass) [ ( "color", toCssString aubergineTheme.text ) ] -- Default Font Color of the theme
    , fc aubergineClass colorTextClass aubergineTheme.text
    , fc aubergineClass colorNoteClass aubergineTheme.note
    , fc aubergineClass colorLinkClass aubergineTheme.link
    , fc aubergineClass colorPrimClass aubergineTheme.prim
    , fc aubergineClass colorSuccClass aubergineTheme.succ
    , fc aubergineClass colorWarnClass aubergineTheme.warn
    , fc aubergineClass colorErrClass aubergineTheme.err
    , ph aubergineClass phColorTextClass (setAlpha 0.3 aubergineTheme.text)
    , ph aubergineClass phColorNoteClass (setAlpha 0.4 aubergineTheme.note)
    ]


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


phColorTextClass : String
phColorTextClass =
    "phtext"


phColorNoteClass : String
phColorNoteClass =
    "phnote"


inlineCodeStyles : List Style
inlineCodeStyles =
    [ s "code" [ ( "border-radius", "0.2em" ) ]
        |> inject monospaceStyle
        |> inject Layout.paddingInlineStyle
    , scoped (c oneDarkClass) "code" [ ( "color", toCssString oneDarkTheme.err ), ( "background-color", toCssString oneDarkTheme.bg ) ]
    , scoped (c aubergineClass) "code" [ ( "color", toCssString aubergineTheme.err ), ( "background-color", toCssString aubergineTheme.bg ) ]
    ]


linkStyles : List Style
linkStyles =
    [ s "a:link" [ ( "text-decoration", "none" ) ] -- Cancelling UA's default decorations
    , s (hov "a:link") [ ( "text-decoration", "underline" ) ]
    , scoped (c oneDarkClass) "a:link" [ ( "color", toCssString oneDarkTheme.link ) ]
    , scoped (c oneDarkClass) "a:visited" [ ( "color", toCssString oneDarkTheme.link ) ]
    , scoped (c aubergineClass) "a:link" [ ( "color", toCssString aubergineTheme.link ) ]
    , scoped (c aubergineClass) "a:visited" [ ( "color", toCssString aubergineTheme.link ) ]
    ]
