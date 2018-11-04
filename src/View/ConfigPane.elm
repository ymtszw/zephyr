module View.ConfigPane exposing (configPaneEl)

import Data.ColorTheme exposing (oneDark)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
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
            [ width (px fixedPaneWidth)
            , height (fill |> maximum m.env.clientHeight)
            , alignLeft
            , padding rectElementOuterPadding
            , scrollbarY
            , BG.color oneDark.bg
            , Font.color oneDark.text
            ]
            (configInnerEl m)

    else
        none


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
        [ configSectionWrapper (ProducerCtrl << Producer.DiscordMsg)
            discordConfigTitleEl
            (discordConfigEl m.producerRegistry.discord)
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


configSectionWrapper : (msg -> Msg) -> Element Msg -> Element msg -> Element Msg
configSectionWrapper tagger titleEl element =
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
        , map tagger element
        ]


sectionTitleFontSize : Int
sectionTitleFontSize =
    scale12 3


sectionBaseFontSize : Int
sectionBaseFontSize =
    scale12 2


discordConfigTitleEl : Element Msg
discordConfigTitleEl =
    row [ spacing spacingUnit ]
        [ iconWithBadgeEl
            { size = sectionTitleFontSize
            , badge = Nothing
            , fallback = "Discord"
            , url = Just (Discord.defaultIconUrl (Just sectionTitleFontSize))
            }
        , text "Discord"
        ]
