module View.Atoms.Input exposing
    ( toggle
    , styles
    )

{-| Input Atoms.

@docs toggle
@docs styles

-}

import Color exposing (cssRgba, setAlpha)
import Html exposing (Attribute, Html, button, div)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Theme exposing (..)
import View.Style exposing (..)


toggle : List (Attribute msg) -> { onChange : Bool -> msg, checked : Bool } -> Html msg
toggle userAttrs opts =
    let
        baseAttrs =
            [ class toggleClass
            , Border.round5
            , attribute "role" "switch"
            , onClick (opts.onChange (not opts.checked))
            ]

        statefulAttrs =
            if opts.checked then
                [ class toggleCheckedClass, Background.colorSucc ]

            else
                [ Background.colorNote ]
    in
    button (baseAttrs ++ statefulAttrs ++ userAttrs)
        [ div [ class toggleHandleClass, Border.round5, Background.colorText ] []
        ]


styles : List Style
styles =
    [ s "input[type=text]"
        [ ( "color", cssRgba oneDarkTheme.text )
        , ( "background-color", cssRgba oneDarkTheme.note )
        ]
    , -- Partial support in Edge, no-support in IE
      s "input[type=text]::placeholder"
        [ ( "color", cssRgba (setAlpha 0.4 oneDarkTheme.text) )
        ]
    , scoped (c aubergineClass)
        "input[type=text]"
        [ ( "color", cssRgba aubergineTheme.text )
        , ( "background-color", cssRgba aubergineTheme.note )
        ]
    , scoped (c aubergineClass)
        "input[type=text]::placeholder"
        [ ( "color", cssRgba (setAlpha 0.4 aubergineTheme.text) )
        ]
    , s (c toggleClass)
        [ ( "width", px (toggleHeight * 2) )
        , ( "height", px toggleHeight )
        , ( "padding", px togglePadding )
        , ( "transition", "background-color " ++ toggleTransitionDuration )
        ]
    , s (c toggleHandleClass)
        [ ( "width", px toggleHandleHeight )
        , ( "height", px toggleHandleHeight )
        , ( "transform", "translateX(0px)" )
        , ( "transition", "transform " ++ toggleTransitionDuration )
        ]
    , s (descOf (c toggleCheckedClass) (c toggleHandleClass))
        [ ( "transform", "translateX(" ++ px (toggleHandleHeight + 1) ++ ")" )
        ]
    ]


toggleClass : String
toggleClass =
    "iptggl"


toggleHeight : Int
toggleHeight =
    18


togglePadding : Int
togglePadding =
    1


toggleTransitionDuration : String
toggleTransitionDuration =
    "0.25s"


toggleCheckedClass : String
toggleCheckedClass =
    "iptgon"


toggleHandleClass : String
toggleHandleClass =
    "iptghnd"


toggleHandleHeight : Int
toggleHandleHeight =
    toggleHeight - (togglePadding * 2)
