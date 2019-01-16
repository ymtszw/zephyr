module View.Molecule.Icon exposing
    ( button, link
    , octiconButton, octiconLink
    , styles
    )

{-| Icon Molecules.

@docs button, link
@docs octiconButton, octiconLink
@docs styles

-}

import Html exposing (Attribute, Html, img)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import Octicons
import View.Atom.Border exposing (round5)
import View.Atom.Button as Button
import View.Atom.Image as Image
import View.Atom.Layout exposing (block, flexBasisAuto, noPadding)
import View.Atom.Typography as Typography
import View.Style exposing (..)


button : List (Attribute msg) -> { onPress : msg, src : String, alt : String } -> Html msg
button attrs opts =
    Html.button ([ class iconClass, onClick opts.onPress, noPadding ] ++ attrs)
        [ img [ src opts.src, alt opts.alt ] [] ]


link : List (Attribute msg) -> { url : String, src : String, alt : String } -> Html msg
link attrs opts =
    Button.link ([ class iconClass, noPadding ] ++ attrs)
        { url = opts.url
        , children = [ img [ src opts.src, alt opts.alt ] [] ]
        }


octiconButton : List (Attribute msg) -> { onPress : msg, size : Int, shape : Octicons.Options -> Html msg } -> Html msg
octiconButton attrs opts =
    Html.button ([ class iconClass, onClick opts.onPress, noPadding ] ++ attrs)
        [ Image.octicon { size = opts.size, shape = opts.shape } ]


octiconLink : List (Attribute msg) -> { url : String, size : Int, shape : Octicons.Options -> Html msg } -> Html msg
octiconLink attrs opts =
    Button.link ([ class iconClass, block, noPadding ] ++ attrs)
        { url = opts.url
        , children = [ Image.octicon { size = opts.size, shape = opts.shape } ]
        }


styles : List Style
styles =
    [ s (c iconClass) [ ( "overflow", "hidden" ) ]
    ]


iconClass : String
iconClass =
    "icon"
