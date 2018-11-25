module View.ConfigPane exposing (configPaneEl)

import Array
import Broker
import Data.ColorTheme exposing (oneDark)
import Data.Column
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Deque
import Dict
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Logger
import Octicons
import StringExtra
import View.ConfigPane.DiscordConfig exposing (discordConfigEl)
import View.Parts exposing (..)


configPaneEl : Model -> Element Msg
configPaneEl m =
    let
        baseAttrs =
            [ width (px fixedPaneWidth)
            , height (fill |> maximum m.env.clientHeight)
            , alignLeft
            , padding rectElementOuterPadding
            , scrollbarY
            , BG.color (setAlpha 0.8 oneDark.bg)
            , Font.color oneDark.text
            , style "transition" "all 0.15s"
            ]

        toggleAttrs =
            if m.viewState.configOpen then
                [ style "visibility" "visible"
                , style "opacity" "1"
                , style "transform" "translateX(0px)"
                ]

            else
                [ style "visibility" "hidden"
                , style "opacity" "0"
                , style "transform" "translateX(-50px)" -- The value sufficient for slide-in effect to be recognizable
                ]
    in
    el (baseAttrs ++ toggleAttrs) (configInnerEl m)


fixedPaneWidth : Int
fixedPaneWidth =
    640


configInnerEl : Model -> Element Msg
configInnerEl m =
    column
        [ width fill
        , height fill
        , spacingXY 0 sectionSpacingY
        ]
        [ configSectionWrapper discordConfigTitleEl <|
            discordConfigEl m.viewState m.producerRegistry.discord
        , configSectionWrapper statusTitleEl <| statusEl m
        , if m.env.isLocalDevelopment then
            el [ width fill, alignBottom, height shrink ] <|
                map LoggerCtrl <|
                    Logger.historyEl m.log

          else
            none
        ]


sectionSpacingY : Int
sectionSpacingY =
    20


configSectionWrapper : Element Msg -> Element Msg -> Element Msg
configSectionWrapper titleEl element =
    column
        [ width fill
        , padding rectElementOuterPadding
        , spacing spacingUnit
        , BG.color oneDark.main
        , BD.rounded rectElementRound
        , Font.size sectionBaseFontSize
        ]
        [ el
            [ width fill
            , Font.bold
            , Font.size sectionTitleFontSize
            , BD.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            ]
            titleEl
        , element
        ]


sectionTitleFontSize : Int
sectionTitleFontSize =
    scale12 3


sectionBaseFontSize : Int
sectionBaseFontSize =
    scale12 2


statusTitleEl : Element Msg
statusTitleEl =
    row [ spacing spacingUnit ]
        [ octiconEl []
            { size = sectionTitleFontSize
            , color = oneDark.succ
            , shape = Octicons.pulse
            }
        , text "Status"
        ]


statusEl : Model -> Element Msg
statusEl m =
    let
        numColumns =
            Dict.size m.columnStore.dict

        numVisible =
            Array.length m.columnStore.order
    in
    column [ padding rectElementInnerPadding, spacing spacingUnit, Font.size statusFontSize ] <|
        List.map (row [ spacing spacingUnit ] << List.map text << List.intersperse "-")
            [ [ "Local message buffer capacity", StringExtra.punctuateNumber <| Broker.capacity m.itemBroker ]
            , [ "Maximum messages per column", StringExtra.punctuateNumber Data.Column.columnItemLimit ]
            , [ "Number of columns", StringExtra.punctuateNumber numColumns ]
            , [ "* Visible columns", StringExtra.punctuateNumber numVisible ]
            , [ "* Shadow columns", StringExtra.punctuateNumber (numColumns - numVisible) ]
            , [ "ClientHeight", StringExtra.punctuateNumber m.env.clientHeight ]
            , [ "ServiceWorker"
              , if m.env.serviceWorkerAvailable then
                    "Registered"

                else
                    "Not available"
              ]
            , [ "IndexedDB"
              , if m.env.indexedDBAvailable then
                    "Used"

                else
                    "Not available"
              ]
            ]


statusFontSize : Int
statusFontSize =
    scale12 1


discordConfigTitleEl : Element Msg
discordConfigTitleEl =
    row [ spacing spacingUnit ]
        [ iconWithBadgeEl []
            { size = sectionTitleFontSize
            , badge = Nothing
            , fallback = "Discord"
            , url = Just (Discord.defaultIconUrl (Just sectionTitleFontSize))
            }
        , text "Discord"
        ]
