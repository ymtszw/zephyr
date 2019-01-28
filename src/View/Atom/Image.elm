module View.Atom.Image exposing
    ( ph
    , octicon, octiconPathStyle, fillPrim, fillSucc, fillWarn, fillErr, fillText
    , styles
    )

{-| Image Atoms.

@docs ph
@docs octicon, octiconPathStyle, fillPrim, fillSucc, fillWarn, fillErr, fillText
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
    "octiconprim"


fillSuccClass : String
fillSuccClass =
    "octiconsucc"


fillWarnClass : String
fillWarnClass =
    "octiconwarn"


fillErrClass : String
fillErrClass =
    "octiconerr"


fillTextClass : String
fillTextClass =
    "octicontext"
