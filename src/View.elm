module View exposing (body)

import Data.Types exposing (Model, Msg)
import Element as E
import Html


body : Model -> Html.Html Msg
body m =
    E.layout [] (bodyElement m)


bodyElement : Model -> E.Element Msg
bodyElement _ =
    E.text "Hi"
