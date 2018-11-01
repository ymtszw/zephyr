module View.ConfigPane exposing (configPaneEl)

import Data.ColorTheme exposing (oneDark)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Logger
import View.ConfigPane.DiscordConfig exposing (discordConfigEl)
import View.Parts exposing (..)


configPaneEl : Model -> Element Msg
configPaneEl m =
    if m.viewState.configOpen then
        el
            [ width (px 640)
            , height (fill |> maximum m.env.clientHeight)
            , alignLeft
            , padding 15
            , scrollbarY
            , BG.color (setAlpha 0.9 oneDark.bg)
            , Font.color oneDark.text
            ]
            (configInnerEl m)

    else
        none


configInnerEl : Model -> Element Msg
configInnerEl m =
    column
        [ width fill
        , height fill
        , spacing 10
        ]
        [ producerConfigsEl m.producerRegistry
        , if m.env.isLocalDevelopment then
            el [ width fill, alignBottom, height shrink ] <|
                map LoggerCtrl <|
                    Logger.historyEl m.log

          else
            none
        ]


producerConfigsEl : ProducerRegistry -> Element Msg
producerConfigsEl producerRegistry =
    column
        [ width fill
        , padding 10
        , spacingXY 0 20
        , BG.color oneDark.main
        , BD.rounded 10
        , Font.size (scale12 2)
        ]
        [ configWrapEl (ProducerCtrl << Producer.DiscordMsg) "Discord" <|
            discordConfigEl producerRegistry.discord
        ]


configWrapEl : (msg -> Msg) -> String -> Element msg -> Element Msg
configWrapEl tagger title element =
    map tagger <|
        column [ width fill, spacingXY 0 5 ]
            [ el
                [ width fill
                , Font.bold
                , Font.size (scale12 3)
                , BD.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                ]
                (text title)
            , element
            ]
