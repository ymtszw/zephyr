module View.Molecules.Icon exposing
    ( rounded14, rounded20, rounded30, rounded40
    , button, link, abbr, imgOrAbbr
    , octiconBlock, octiconButton, octiconLink
    , rehydrateButton, pinBadge14, botBadge14, discord10, slack10, discord14, slack14, discord20, slack20
    , discordImageUrl20, discordImageUrl40, discordImageUrlWithFallback40
    , styles
    )

{-| Icon Molecules.

@docs rounded14, rounded20, rounded30, rounded40
@docs button, link, abbr, imgOrAbbr
@docs octiconBlock, octiconButton, octiconLink
@docs rehydrateButton, pinBadge14, botBadge14, discord10, slack10, discord14, slack14, discord20, slack20
@docs discordImageUrl20, discordImageUrl40, discordImageUrlWithFallback40
@docs styles

-}

import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Attribute, Html, div, img)
import Html.Attributes exposing (alt, class, disabled, src, title)
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


{-| Border-rounded 14x14 sized icon.
-}
rounded14 : Attribute msg
rounded14 =
    class rounded14Class


{-| Border-rounded 20x20 sized icon. Widely used.
-}
rounded20 : Attribute msg
rounded20 =
    class rounded20Class


{-| Border-rounded 30x30 sized icon. Widely used.
-}
rounded30 : Attribute msg
rounded30 =
    class rounded30Class


{-| Border-rounded 40x40 sized icon. Widely used.
-}
rounded40 : Attribute msg
rounded40 =
    class rounded40Class


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
    centeredBlock attrs (t (String.left 1 desc))


centeredBlock : List (Attribute msg) -> Html msg -> Html msg
centeredBlock attrs content =
    div
        ([ class iconClass
         , flexColumn
         , flexCenter
         , Border.solid
         , Border.w1
         ]
            ++ attrs
        )
        [ div [] [ content ]
        ]


imgOrAbbr : List (Attribute msg) -> String -> Maybe String -> Html msg
imgOrAbbr attrs desc srcMaybe =
    case srcMaybe of
        Just src_ ->
            img ([ class iconClass, src src_, alt desc ] ++ attrs) []

        Nothing ->
            abbr attrs desc


octiconBlock : List (Attribute msg) -> { size : Int, shape : Octicons.Options -> Html msg } -> Html msg
octiconBlock attrs opts =
    centeredBlock attrs (Image.octicon { size = opts.size, shape = opts.shape })


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


pinBadge14 : Html msg
pinBadge14 =
    div
        [ class rounded14Class
        , title "Pinned"
        , Image.fillWarn
        , Image.rotate45
        ]
        [ Image.octicon { size = rounded14Size, shape = Octicons.pin } ]


botBadge14 : Html msg
botBadge14 =
    div
        [ class rounded14Class
        , title "BOT"
        , Image.fillText
        , Background.colorSucc
        ]
        [ Image.octicon { size = rounded14Size, shape = Octicons.zap } ]


discord10 : List (Attribute msg) -> Html msg
discord10 attrs =
    logo attrs rounded10Class "Discord logo" <| Discord.defaultIconUrl (Just rounded10Size)


slack10 : List (Attribute msg) -> Html msg
slack10 attrs =
    logo attrs rounded10Class "Slack logo" <| Slack.defaultIconUrl (Just rounded10Size)


discord14 : List (Attribute msg) -> Html msg
discord14 attrs =
    logo attrs rounded14Class "Discord logo" <| Discord.defaultIconUrl (Just rounded14Size)


slack14 : List (Attribute msg) -> Html msg
slack14 attrs =
    logo attrs rounded14Class "Slack logo" <| Slack.defaultIconUrl (Just rounded14Size)


discord20 : List (Attribute msg) -> Html msg
discord20 attrs =
    logo attrs rounded20Class "Discord logo" <| Discord.defaultIconUrl (Just rounded20Size)


slack20 : List (Attribute msg) -> Html msg
slack20 attrs =
    logo attrs rounded20Class "Slack logo" <| Slack.defaultIconUrl (Just rounded20Size)


logo : List (Attribute msg) -> String -> String -> String -> Html msg
logo attrs sizeClass alt_ src_ =
    img ([ class sizeClass, src src_, alt alt_ ] ++ attrs) []


discordImageUrl20 : Discord.Image -> String
discordImageUrl20 =
    Discord.imageUrlNoFallback (Just rounded20Size)


discordImageUrl40 : Discord.Image -> String
discordImageUrl40 =
    Discord.imageUrlNoFallback (Just rounded40Size)


discordImageUrlWithFallback40 : String -> Maybe Discord.Image -> String
discordImageUrlWithFallback40 =
    Discord.imageUrlWithFallback (Just rounded40Size)



-- STYLES


styles : List Style
styles =
    [ s (c iconClass)
        [ ( "overflow", "hidden" )
        , ( "flex-basis", "auto!important" )
        , ( "justify-content", "center" )
        , ( "user-select", "none" )
        ]
    , s (c rounded10Class)
        [ ( "width", px rounded10Size )
        , ( "height", px rounded10Size )
        , ( "flex-basis", "auto" )
        ]
        |> inject Border.round2Style
    , s (c rounded14Class)
        [ ( "width", px rounded14Size )
        , ( "height", px rounded14Size )
        , ( "flex-basis", "auto" )
        ]
        |> inject Border.round2Style
    , s (c rounded20Class)
        [ ( "width", px rounded20Size )
        , ( "height", px rounded20Size )
        , ( "flex-basis", "auto" )
        ]
        |> inject Border.round2Style
    , s (c rounded30Class)
        [ ( "width", px rounded30Size )
        , ( "height", px rounded30Size )
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


rounded10Class : String
rounded10Class =
    "iconr10"


rounded10Size : Int
rounded10Size =
    10


rounded14Class : String
rounded14Class =
    "iconr14"


rounded14Size : Int
rounded14Size =
    14


rounded20Class : String
rounded20Class =
    "iconr20"


rounded20Size : Int
rounded20Size =
    20


rounded30Class : String
rounded30Class =
    "iconr30"


rounded30Size : Int
rounded30Size =
    30


rounded40Class : String
rounded40Class =
    "iconr40"


rounded40Size : Int
rounded40Size =
    40
