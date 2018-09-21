module View.Parts exposing (disabled, disabledColor)

import Data.ColorTheme exposing (oneDark)
import Element as El
import Element.Background as BG
import Html.Attributes


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
        [ BG.color oneDark.sub ] ++ attrs

    else
        [ BG.color oneDark.succ ] ++ attrs
