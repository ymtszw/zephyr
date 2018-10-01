module View.Parts exposing (disabled, disabledColor, octiconEl)

import Data.ColorTheme exposing (oneDark)
import Element as El exposing (Element)
import Element.Background as BG
import Element.Font
import Html
import Html.Attributes
import Octicons


disabled : Bool -> List (El.Attribute msg) -> List (El.Attribute msg)
disabled isDisabled attrs =
    if isDisabled then
        [ El.htmlAttribute (Html.Attributes.disabled isDisabled)
        , El.htmlAttribute (Html.Attributes.style "cursor" "not-allowed")
        ]
            ++ attrs

    else
        attrs


disabledColor : Bool -> List (El.Attribute msg) -> List (El.Attribute msg)
disabledColor isDisabled attrs =
    if isDisabled then
        [ BG.color oneDark.sub, Element.Font.color oneDark.note ] ++ attrs

    else
        [ BG.color oneDark.active ] ++ attrs


octiconEl : (Octicons.Options -> Html.Html msg) -> Element msg
octiconEl octicon =
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
        |> Octicons.size 26
        |> octicon
        |> El.html
