module View.Atoms.Animation exposing
    ( rotating, slideDown
    , styles
    )

{-| Perpetual animation Atoms.

@docs rotating, slideDown
@docs styles

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)
import View.Style exposing (..)


rotating : Attribute msg
rotating =
    class (animClass rotatingKeyframesName)


slideDown : Attribute msg
slideDown =
    class (animClass slideDownKeyframesName)


styles : List Style
styles =
    [ s (c (animClass rotatingKeyframesName)) [ ( "animation", "1.5s linear 0s infinite " ++ rotatingKeyframesName ) ]
    , kf rotatingKeyframesName
        [ ( "from", [ ( "transform", "rotate(0turn)" ) ] )
        , ( "to", [ ( "transform", "rotate(1turn)" ) ] )
        ]
    , s (c (animClass slideDownKeyframesName)) [ ( "animation", "1s ease-out 0s infinite " ++ slideDownKeyframesName ) ]
    , kf slideDownKeyframesName
        [ ( "from", [ ( "transform", "translateY(-100%)" ) ] )
        , ( "to", [ ( "transform", "translateY(0)" ) ] )
        ]
    ]


animClass : String -> String
animClass kfName =
    "anim-" ++ kfName


rotatingKeyframesName : String
rotatingKeyframesName =
    "rotating"


slideDownKeyframesName : String
slideDownKeyframesName =
    "slideDown"
