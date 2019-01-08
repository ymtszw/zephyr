module View.Atom.Image exposing
    ( block
    , ph
    , styles
    )

{-| Image Atoms.

@docs block
@docs ph
@docs styles

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import Url.Builder
import View.Style exposing (..)


block : Attribute msg
block =
    class blockClass


{-| Generates a link to placeholder image, powered by Lorem Picsum.

<https://picsum.photos/>

-}
ph : Int -> Int -> String
ph width height =
    Url.Builder.crossOrigin "https://picsum.photos"
        [ String.fromInt width, String.fromInt height ]
        []


styles : List Style
styles =
    [ s "img" [ ( "vertical-align", "middle" ) ]
    , c blockClass [ ( "display", "block" ) ]
    ]


blockClass : String
blockClass =
    "imgbl"
