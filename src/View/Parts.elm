module View.Parts exposing (disabled, disabledColor, noneAttr, octiconEl, octiconFreeSizeEl, scale12, squareIconEl)

import Data.ColorTheme exposing (css, oneDark)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Html
import Html.Attributes
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


squareIconEl : Int -> String -> Maybe String -> Element msg
squareIconEl size name urlMaybe =
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



-- FONT SIZE


scale12 : Int -> Int
scale12 =
    modular 12 1.25 >> round
