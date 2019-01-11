module View.Atom.Input exposing
    ( toggle
    , styles
    )

{-| Input Atoms.

@docs toggle
@docs styles

-}

import Color exposing (cssRgba)
import Element.Background as Background
import Html exposing (Attribute, Html, button, div)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import View.Atom.Border as Border
import View.Atom.Theme exposing (..)
import View.Style exposing (..)


toggle : List (Attribute msg) -> { onChange : Bool -> msg, checked : Bool } -> Html msg
toggle userAttrs opts =
    button
        ([ class toggleClass
         , Border.round5
         , attribute "role" "switch"
         , if opts.checked then
            class toggleCheckedClass

           else
            noAttr
         , onClick (opts.onChange (not opts.checked))
         ]
            ++ userAttrs
        )
        [ div [ class toggleHandleClass, Border.round5 ] []
        ]


styles : List Style
styles =
    [ s (c toggleClass)
        [ ( "width", px (toggleHeight * 2) )
        , ( "height", px toggleHeight )
        , ( "padding", px togglePadding )
        ]
    , s (c oneDarkClass ++ " " ++ c toggleClass)
        [ ( "background-color", cssRgba oneDarkTheme.note ) ]
    , s (c aubergineClass ++ " " ++ c toggleClass)
        [ ( "background-color", cssRgba aubergineTheme.note ) ]
    , s (c oneDarkClass ++ " " ++ c toggleClass ++ c toggleCheckedClass)
        [ ( "background-color", cssRgba oneDarkTheme.succ ) ]
    , s (c aubergineClass ++ " " ++ c toggleClass ++ c toggleCheckedClass)
        [ ( "background-color", cssRgba aubergineTheme.succ ) ]
    , s (c toggleHandleClass)
        [ ( "width", px toggleHandleHeight )
        , ( "height", px toggleHandleHeight )
        , ( "transform", "translateX(0px)" )
        , ( "transition", "transform 0.25s" )
        ]
    , s (c oneDarkClass ++ " " ++ c toggleHandleClass) [ ( "background-color", cssRgba oneDarkTheme.text ) ]
    , s (c aubergineClass ++ " " ++ c toggleHandleClass) [ ( "background-color", cssRgba aubergineTheme.text ) ]
    , s (c toggleCheckedClass ++ " " ++ c toggleHandleClass)
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


toggleCheckedClass : String
toggleCheckedClass =
    "iptgon"


toggleHandleClass : String
toggleHandleClass =
    "iptghnd"


toggleHandleHeight : Int
toggleHandleHeight =
    toggleHeight - (togglePadding * 2)
