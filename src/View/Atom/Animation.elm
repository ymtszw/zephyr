module View.Atom.Animation exposing
    ( rotating
    , styles
    )

{-| Animation Atom.

@docs rotating
@docs styles

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Style exposing (..)


rotating : Attribute msg
rotating =
    class rotatingClass


styles : List Style
styles =
    [ s (c rotatingClass) [ ( "animation", "1.5s linear 0s infinite " ++ rotatingKeyframesName ) ]
    , kf rotatingKeyframesName
        [ ( "from", [ ( "transform", "rotate(0turn)" ) ] )
        , ( "to", [ ( "transform", "rotate(1turn)" ) ] )
        ]
    ]


rotatingClass : String
rotatingClass =
    "anim-rotating"


rotatingKeyframesName : String
rotatingKeyframesName =
    "rotating"
