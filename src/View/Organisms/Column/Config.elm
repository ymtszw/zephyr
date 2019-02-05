module View.Organisms.Column.Config exposing (Effects, render)

import Html exposing (Html, div)


type alias Effects msg =
    { onCloseButtonClick : msg
    }


render : Effects msg -> Html msg
render eff =
    div [] []
