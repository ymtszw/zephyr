module View.Parts exposing (disabled, disabledColor, octiconEl, octiconFreeSizeEl, scale12, squareIconEl)

import Data.ColorTheme exposing (oneDark)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Html
import Html.Attributes
import Octicons


disabled : Bool -> List (El.Attribute msg) -> List (El.Attribute msg)
disabled isDisabled attrs =
    if isDisabled then
        [ El.htmlAttribute (Html.Attributes.disabled isDisabled)
        , El.htmlAttribute (Html.Attributes.style "cursor" "default")
        ]
            ++ attrs

    else
        attrs


disabledColor : Bool -> List (El.Attribute msg) -> List (El.Attribute msg)
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
    let
        { red, green, blue } =
            El.toRgb oneDark.note

        colorStr =
            "rgb("
                ++ String.fromFloat (255 * red)
                ++ ","
                ++ String.fromFloat (255 * green)
                ++ ","
                ++ String.fromFloat (255 * blue)
                ++ ")"
    in
    Octicons.defaultOptions
        |> Octicons.color colorStr
        |> Octicons.size size
        |> octicon
        |> El.html


squareIconEl : String -> Maybe String -> Element msg
squareIconEl name urlMaybe =
    let
        ( attr, el ) =
            case urlMaybe of
                Just url ->
                    ( BG.uncropped url, El.none )

                Nothing ->
                    ( Font.size (scale12 4), El.el [ El.centerX, El.centerY ] (El.text (String.left 1 name)) )
    in
    El.el
        [ BG.color oneDark.bg
        , El.width (El.px 50)
        , El.height (El.px 50)
        , El.alignTop
        , BD.rounded 5
        , El.htmlAttribute (Html.Attributes.title name)
        , El.pointer
        , attr
        ]
        el



-- FONT SIZE


scale12 : Int -> Int
scale12 =
    El.modular 12 1.25 >> round
