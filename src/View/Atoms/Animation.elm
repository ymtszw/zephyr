module View.Atoms.Animation exposing
    ( rotating, slideDown, borderFlash
    , styles
    )

{-| Perpetual animation Atoms.

@docs rotating, slideDown, borderFlash
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


borderFlash : Attribute msg
borderFlash =
    class (animClass borderFlashKeyframesName)


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
    , s (c (animClass borderFlashKeyframesName)) [ ( "animation", "0.5s ease-out 0s 4 alternate " ++ borderFlashKeyframesName ) ]
    , kf borderFlashKeyframesName
        [ ( "from", [ ( "border-color", "rgba(220,221,222,0)" ) ] )
        , ( "to", [ ( "border-color", "rgba(220,221,222,1)" ) ] )
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


borderFlashKeyframesName : String
borderFlashKeyframesName =
    "borderFlash"
