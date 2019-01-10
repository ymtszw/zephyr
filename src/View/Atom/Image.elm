module View.Atom.Image exposing
    ( ph
    , octicon, octiconPathStyle
    , styles
    )

{-| Image Atoms.

@docs ph
@docs octicon, octiconPathStyle
@docs styles

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute, Html)
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


styles : List Style
styles =
    [ s "img" [ ( "vertical-align", "middle" ) ]
    , s (c octiconClass) [ ( "vertical-align", "middle" ) ]
    , s (String.join "," octiconPaths) [ ( "fill", cssRgba oneDarkTheme.note ) ] -- Default fill color
    , octiconPathStyle (c oneDarkClass) [ ( "fill", cssRgba oneDarkTheme.note ) ]
    , octiconPathStyle (c aubergineClass) [ ( "fill", cssRgba aubergineTheme.note ) ]
    ]


octiconPaths : List String
octiconPaths =
    [ c octiconClass ++ " path", c octiconClass ++ " polygon" ]


octiconPathStyle : String -> List ( String, String ) -> Style
octiconPathStyle scopeSelector props =
    let
        selector =
            String.join "," <|
                List.map ((++) (scopeSelector ++ " ")) <|
                    octiconPaths
    in
    s selector props


octiconClass : String
octiconClass =
    "octicon"
