module View.Parts exposing
    ( noneAttr, breakP, breakT, breakTColumn, collapsingColumn, dragHandle
    , octiconEl, octiconFreeSizeEl, squareIconOrHeadEl
    , disabled, disabledColor, scale12, css, brightness, setAlpha, manualStyle
    , discordGuildIconEl
    , fixedColumnWidth
    )

{-| View parts, complementing Element and Html.


## Essenstials

@docs noneAttr, breakP, breakT, breakTColumn, collapsingColumn, dragHandle


## Icons

@docs octiconEl, octiconFreeSizeEl, squareIconOrHeadEl


## Styles

@docs disabled, disabledColor, scale12, css, brightness, setAlpha, manualStyle


## Discord

@docs discordGuildIconEl


## Constants

@docs fixedColumnWidth

-}

import Data.ColorTheme exposing (oneDark)
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Html
import Html.Attributes exposing (class, draggable, style)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode
import Octicons


noneAttr : Attribute msg
noneAttr =
    htmlAttribute (Html.Attributes.property "none" Json.Encode.null)


disabled : Bool -> List (Attribute msg) -> List (Attribute msg)
disabled isDisabled attrs =
    if isDisabled then
        [ htmlAttribute (Html.Attributes.disabled isDisabled)
        , htmlAttribute (Html.Attributes.style "cursor" "default")
        ]
            ++ attrs

    else
        attrs


disabledColor : Bool -> List (Attribute msg) -> List (Attribute msg)
disabledColor isDisabled attrs =
    if isDisabled then
        [ BG.color oneDark.sub, Font.color oneDark.note ] ++ attrs

    else
        [ BG.color oneDark.active ] ++ attrs


octiconEl : (Octicons.Options -> Html.Html msg) -> Element msg
octiconEl =
    octiconFreeSizeEl 26


octiconFreeSizeEl : Int -> (Octicons.Options -> Html.Html msg) -> Element msg
octiconFreeSizeEl size octicon =
    Octicons.defaultOptions
        |> Octicons.color (css oneDark.note)
        |> Octicons.size size
        |> octicon
        |> html


squareIconOrHeadEl : Int -> String -> Maybe String -> Element msg
squareIconOrHeadEl size name urlMaybe =
    let
        ( attr, fallbackContent ) =
            case urlMaybe of
                Just url ->
                    ( BG.uncropped url, none )

                Nothing ->
                    ( Font.size (size // 2), el [ centerX, centerY ] (text (String.left 1 name)) )
    in
    el
        [ BG.color oneDark.bg
        , width (px size)
        , height (px size)
        , alignTop
        , BD.rounded 5
        , htmlAttribute (Html.Attributes.title name)
        , attr
        ]
        fallbackContent


{-| Text that can break on parent inline element width.
Respects "word-break" and "white-space" styles.

This is a workaround for <https://github.com/mdgriffith/elm-ui/issues/49>

-}
breakT : String -> Element msg
breakT =
    Html.text >> html


{-| `paragraph` with "word-break: break-all" and "white-space: pre-wrap".

Suitable for user-generated texts. Use with `breakT`.

-}
breakP : List (Attribute msg) -> List (Element msg) -> Element msg
breakP attrs =
    paragraph <| attrs ++ [ htmlAttribute (class breakClassName) ]


breakClassName : String
breakClassName =
    "breakEl"


{-| `textColumn` with "word-break: break-all" and "white-space: pre-wrap".
-}
breakTColumn : List (Attribute msg) -> List (Element msg) -> Element msg
breakTColumn attrs =
    textColumn <| htmlAttribute (class breakClassName) :: attrs


collapsingColumn : List (Attribute msg) -> List (Element msg) -> Element msg
collapsingColumn attrs elements =
    case elements of
        [] ->
            none

        _ ->
            column attrs elements



-- STYLE HELPER


scale12 : Int -> Int
scale12 =
    modular 12 1.25 >> round


{-| Shift brightness of a Color (RGB) by a power of 1.15, without altering alpha.

`brightness 1` on a Color of `{red = 100, green = 100, blue = 100}`
will yield `{red = 115, green = 115, blue = 115}`.

`brightness -1` on the same Color will yield `{red = 86.96, green = 86.96, blue = 86.96}`

-}
brightness : Float -> Color -> Color
brightness power color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgb (red * (1.15 ^ power)) (green * (1.15 ^ power)) (blue * (1.15 ^ power))


setAlpha : Float -> Color -> Color
setAlpha a color =
    let
        rgba =
            toRgb color
    in
    fromRgb { rgba | alpha = a }


{-| Dump a Color to CSS-compatible representaiton
-}
css : Color -> String
css color =
    let
        { red, green, blue } =
            toRgb color
    in
    String.join ""
        [ "rgb("
        , String.fromFloat (255 * red)
        , ","
        , String.fromFloat (255 * green)
        , ","
        , String.fromFloat (255 * blue)
        , ")"
        ]


dragHandle : Decoder msg -> List (Attribute msg)
dragHandle onDragstart =
    [ htmlAttribute (draggable "true")
    , htmlAttribute (class dragHandleClassName)
    , htmlAttribute (Html.Events.stopPropagationOn "dragstart" (D.map (\msg -> ( msg, True )) onDragstart))
    ]


dragHandleClassName : String
dragHandleClassName =
    "dragHandle"


discordGuildIconEl : Int -> Discord.Guild -> Element msg
discordGuildIconEl size guild =
    squareIconOrHeadEl size guild.name (Maybe.map (Discord.imageUrlNoFallback (Just (nextPowerOfTwoString size))) guild.icon)


nextPowerOfTwoString : Int -> String
nextPowerOfTwoString size =
    if size > 512 then
        "1024"

    else if size > 256 then
        "512"

    else if size > 128 then
        "256"

    else if size > 64 then
        "128"

    else if size > 32 then
        "64"

    else if size > 16 then
        "32"

    else
        "16"



-- MANUAL STYLE


manualStyle : Html.Html msg
manualStyle =
    Html.node "style"
        []
        [ Html.text "::-webkit-scrollbar{display:none;}"
        , Html.text <| "." ++ breakClassName ++ "{white-space:pre-wrap!important;word-break:break-all!important;}"
        , Html.text <| "." ++ dragHandleClassName ++ "{cursor:all-scroll;}"
        ]



-- CONSTANTS


fixedColumnWidth : Int
fixedColumnWidth =
    350
