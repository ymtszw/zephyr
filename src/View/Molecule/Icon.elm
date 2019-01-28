module View.Molecule.Icon exposing
    ( rounded40, size40
    , button, link, abbr, imgOrAbbr
    , octiconButton, octiconLink
    , styles
    )

{-| Icon Molecules.

@docs rounded40, size40
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


{-| Border-rounded 40x40 sized icon. Widely used.
-}
rounded40 : Attribute msg
rounded40 =
    class rounded40Class


{-| Constant of `Just 40`, should be inserted to CDN APIs.
-}
size40 : Maybe Int
size40 =
    Just rounded40Size


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
        , ( "user-select", "none" )
        ]
    , s (c rounded40Class)
        [ ( "width", px rounded40Size )
        , ( "height", px rounded40Size )
        , ( "flex-basis", "auto" )
        ]
        |> inject Border.round5Style
    ]


iconClass : String
iconClass =
    "icon"


rounded40Class : String
rounded40Class =
    "iconr40"


rounded40Size : Int
rounded40Size =
    40
