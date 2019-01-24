module View.Molecule.Icon exposing
    ( button, link, abbr, imgOrAbbr
    , octiconButton, octiconLink
    , styles
    )

{-| Icon Molecules.

@docs button, link, abbr, imgOrAbbr
@docs octiconButton, octiconLink
@docs styles

-}

import Html exposing (Attribute, Html, div, img)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import Octicons
import View.Atom.Border as Border
import View.Atom.Button as Button
import View.Atom.Image as Image
import View.Atom.Layout exposing (..)
import View.Atom.Typography exposing (serif, sizeTitle, t)
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


abbr : List (Attribute msg) -> String -> Html msg
abbr attrs desc =
    div
        ([ class iconClass
         , flexColumn
         , flexCenter
         , Border.solid
         , Border.w1
         ]
            ++ attrs
        )
        [ div [] [ t (String.left 1 desc) ]
        ]


imgOrAbbr : List (Attribute msg) -> String -> Maybe String -> Html msg
imgOrAbbr attrs desc urlMaybe =
    case urlMaybe of
        Just url ->
            img ([ class iconClass, src url, alt desc ] ++ attrs) []

        Nothing ->
            abbr attrs desc


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
    [ s (c iconClass)
        [ ( "overflow", "hidden" )
        , ( "flex-basis", "auto!important" )
        , ( "justify-content", "center" )
        ]
    ]


iconClass : String
iconClass =
    "icon"
