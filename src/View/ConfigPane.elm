module View.ConfigPane exposing (configPaneEl)

import Data.ColorTheme exposing (oneDark)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer
import Element exposing (..)
import Element.Font as Font
import Logger


configPaneEl : Model -> Element Msg
configPaneEl m =
    el
        [ width (fill |> minimum 480 |> maximum 860)
        , height (fill |> maximum m.env.clientHeight)
        , padding 15
        , scrollbarY
        , Font.color oneDark.text
        ]
        (configInnerEl m)


configInnerEl : Model -> Element Msg
configInnerEl m =
    column
        [ width fill
        , height fill
        , spacing 10
        ]
        [ map ProducerCtrl <| Producer.configsEl m.producerRegistry
        , if m.env.isLocalDevelopment then
            el [ width fill, alignBottom, height shrink ] <|
                map LoggerCtrl <|
                    Logger.historyEl m.log

          else
            none
        ]
