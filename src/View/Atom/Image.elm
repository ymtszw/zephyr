module View.Atom.Image exposing
    ( ph
    , octicon
    , styles
    )

{-| Image Atoms.

@docs ph
@docs octicon
@docs styles

-}

import Color exposing (Color, cssRgba)
import Html exposing (Attribute, Html)
import Octicons
import Url.Builder
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
octicon : { size : Int, color : Color, shape : Octicons.Options -> Html msg } -> Html msg
octicon opts =
    Octicons.defaultOptions
        |> Octicons.color (cssRgba opts.color)
        |> Octicons.size opts.size
        |> opts.shape


styles : List Style
styles =
    [ s "img" [ ( "vertical-align", "middle" ) ]
    , c "octicon" [ ( "vertical-align", "middle" ) ]
    ]
