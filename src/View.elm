module View exposing (body)

import Data.Types exposing (Model, Msg)
import Element as El
import Html


body : Model -> Html.Html Msg
body m =
    El.layout [] (bodyElement m)


bodyElement : Model -> El.Element Msg
bodyElement { testKey } =
    El.text testKey
