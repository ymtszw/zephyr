module View.Atom.Image exposing
    ( ph
    , octicon, octiconPathStyle, fillPrim, fillSucc, fillWarn, fillErr, fillText
    , hovPrim, hovSucc, hovWarn, hovErr, hovText
    , rotate45
    , styles
    )

{-| Image Atoms.

@docs ph
@docs octicon, octiconPathStyle, fillPrim, fillSucc, fillWarn, fillErr, fillText
@docs hovPrim, hovSucc, hovWarn, hovErr, hovText
@docs rotate45
@docs styles

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)
import Octicons
import Url.Builder
import View.Atom.Theme exposing (..)
import View.Style exposing (..)


{-| Generates a link to placeholder image, powered by Lorem Picsum.

<https://picsum.photos/>

-}
ph : Int -> Int -> String
ph width height =
    Url.Builder.crossOrigin "https://picsum.photos"
        [ String.fromInt width, String.fromInt height ]
        []


{-| Render inline Octicon. Has `octicon` class.
-}
octicon : { size : Int, shape : Octicons.Options -> Html msg } -> Html msg
octicon opts =
    Octicons.defaultOptions
        |> Octicons.size opts.size
        |> opts.shape


fillPrim : Attribute msg
fillPrim =
    class fillPrimClass


fillSucc : Attribute msg
fillSucc =
    class fillSuccClass


fillWarn : Attribute msg
fillWarn =
    class fillWarnClass


fillErr : Attribute msg
fillErr =
    class fillErrClass


fillText : Attribute msg
fillText =
    class fillTextClass


hovPrim : Attribute msg
hovPrim =
    class hovPrimClass


hovSucc : Attribute msg
hovSucc =
    class hovSuccClass


hovWarn : Attribute msg
hovWarn =
    class hovWarnClass


hovErr : Attribute msg
hovErr =
    class hovErrClass


hovText : Attribute msg
hovText =
    class hovTextClass


rotate45 : Attribute msg
rotate45 =
    class rotate45Class



-- STYLES


styles : List Style
styles =
    [ s "img" [ ( "vertical-align", "middle" ) ]
    , s (c octiconClass) [ ( "vertical-align", "middle" ) ]
    , s (String.join "," octiconPaths) [ ( "fill", cssRgba oneDarkTheme.note ) ] -- Default fill color
    , octiconPathStyle (c oneDarkClass) [ ( "fill", cssRgba oneDarkTheme.note ) ]
    , octiconPathStyle (c aubergineClass) [ ( "fill", cssRgba aubergineTheme.note ) ]
    , scopedFillStyle (c oneDarkClass) (c fillPrimClass) oneDarkTheme.prim
    , scopedFillStyle (c oneDarkClass) (c fillSuccClass) oneDarkTheme.succ
    , scopedFillStyle (c oneDarkClass) (c fillWarnClass) oneDarkTheme.warn
    , scopedFillStyle (c oneDarkClass) (c fillErrClass) oneDarkTheme.err
    , scopedFillStyle (c oneDarkClass) (c fillTextClass) oneDarkTheme.text
    , scopedFillStyle (c aubergineClass) (c fillPrimClass) aubergineTheme.prim
    , scopedFillStyle (c aubergineClass) (c fillSuccClass) aubergineTheme.succ
    , scopedFillStyle (c aubergineClass) (c fillWarnClass) aubergineTheme.warn
    , scopedFillStyle (c aubergineClass) (c fillErrClass) aubergineTheme.err
    , scopedFillStyle (c aubergineClass) (c fillTextClass) aubergineTheme.text
    , scopedFillStyle (c oneDarkClass) (hov (c hovPrimClass)) oneDarkTheme.prim
    , scopedFillStyle (c oneDarkClass) (hov (c hovSuccClass)) oneDarkTheme.succ
    , scopedFillStyle (c oneDarkClass) (hov (c hovWarnClass)) oneDarkTheme.warn
    , scopedFillStyle (c oneDarkClass) (hov (c hovErrClass)) oneDarkTheme.err
    , scopedFillStyle (c oneDarkClass) (hov (c hovTextClass)) oneDarkTheme.text
    , scopedFillStyle (c aubergineClass) (hov (c hovPrimClass)) aubergineTheme.prim
    , scopedFillStyle (c aubergineClass) (hov (c hovSuccClass)) aubergineTheme.succ
    , scopedFillStyle (c aubergineClass) (hov (c hovWarnClass)) aubergineTheme.warn
    , scopedFillStyle (c aubergineClass) (hov (c hovErrClass)) aubergineTheme.err
    , scopedFillStyle (c aubergineClass) (hov (c hovTextClass)) aubergineTheme.text
    , s (c rotate45Class) [ ( "transform", "rotate(-45deg)" ) ]
    ]


octiconPaths : List String
octiconPaths =
    [ c octiconClass ++ " path", c octiconClass ++ " polygon" ]


octiconClass : String
octiconClass =
    "octicon"


octiconPathStyle : String -> List ( String, String ) -> Style
octiconPathStyle scopeSelector props =
    let
        selector =
            String.join "," <|
                List.map ((++) (scopeSelector ++ " ")) <|
                    octiconPaths
    in
    s selector props


scopedFillStyle : String -> String -> Color -> Style
scopedFillStyle themeSelector targetSelector color =
    octiconPathStyle (themeSelector ++ " " ++ targetSelector) [ ( "fill", cssRgba color ) ]


fillPrimClass : String
fillPrimClass =
    "octiconfillprim"


fillSuccClass : String
fillSuccClass =
    "octiconfillsucc"


fillWarnClass : String
fillWarnClass =
    "octiconfillwarn"


fillErrClass : String
fillErrClass =
    "octiconfillerr"


fillTextClass : String
fillTextClass =
    "octiconfilltext"


hovPrimClass : String
hovPrimClass =
    "octiconhovprim"


hovSuccClass : String
hovSuccClass =
    "octiconhovsucc"


hovWarnClass : String
hovWarnClass =
    "octiconhovwarn"


hovErrClass : String
hovErrClass =
    "octiconhoverr"


hovTextClass : String
hovTextClass =
    "octiconhovtext"


rotate45Class : String
rotate45Class =
    "rot45"
