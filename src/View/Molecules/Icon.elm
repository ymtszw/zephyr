module View.Molecules.Icon exposing
    ( rounded20, rounded40, size20, size40
    , button, link, abbr, imgOrAbbr
    , octiconButton, octiconLink
    , rehydrateButton
    , styles
    )

{-| Icon Molecules.

@docs rounded20, rounded40, size20, size40
@docs button, link, abbr, imgOrAbbr
@docs octiconButton, octiconLink
@docs rehydrateButton
@docs styles

-}

import Html exposing (Attribute, Html, div, img)
import Html.Attributes exposing (alt, class, disabled, src)
import Html.Events exposing (onClick)
import Octicons
import View.Atoms.Animation as Animation
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Button as Button
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (t)
import View.Style exposing (..)


{-| Border-rounded 20x20 sized icon. Widely used.
-}
rounded20 : Attribute msg
rounded20 =
    class rounded20Class


{-| Border-rounded 40x40 sized icon. Widely used.
-}
rounded40 : Attribute msg
rounded40 =
    class rounded40Class


{-| Constant of `Just 20`, should be inserted to CDN APIs.
-}
size20 : Maybe Int
size20 =
    Just rounded20Size


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


{-| Used in Producers' Organisms. Fixed 20px size.
-}
rehydrateButton : msg -> Bool -> Html msg
rehydrateButton onPress rehydrating =
    let
        rehydrateButtonSize =
            26
    in
    octiconButton
        [ alignStart
        , disabled rehydrating
        , Border.elliptic
        , Background.transparent
        , Image.fillPrim
        , if rehydrating then
            Animation.rotating

          else
            noAttr
        ]
        { onPress = onPress
        , size = rehydrateButtonSize
        , shape = Octicons.sync
        }



-- STYLES


styles : List Style
styles =
    [ s (c iconClass)
        [ ( "overflow", "hidden" )
        , ( "flex-basis", "auto!important" )
        , ( "justify-content", "center" )
        , ( "user-select", "none" )
        ]
    , s (c rounded20Class)
        [ ( "width", px rounded20Size )
        , ( "height", px rounded20Size )
        , ( "flex-basis", "auto" )
        ]
        |> inject Border.round2Style
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


rounded20Class : String
rounded20Class =
    "iconr20"


rounded20Size : Int
rounded20Size =
    20
