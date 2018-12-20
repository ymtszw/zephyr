module View.ConfigPane exposing (configPaneEl)

import Array
import Broker
import Data.ColorTheme exposing (ColorTheme, aubergine, oneDark)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Pref as Pref exposing (Pref)
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Data.ProducerRegistry exposing (ProducerRegistry)
import Deque
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Keyed
import Logger
import Octicons
import StringExtra
import View.ConfigPane.DiscordConfig exposing (discordConfigEl)
import View.ConfigPane.SlackConfig exposing (slackConfigEl)
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
        [ configSectionWrapper oneDark prefTitleEl <|
            prefEl m.pref m.columnStore
        , configSectionWrapper aubergine slackConfigTitleEl <|
            slackConfigEl m.viewState m.producerRegistry.slack
        , configSectionWrapper oneDark discordConfigTitleEl <|
            discordConfigEl m.viewState m.producerRegistry.discord
        , configSectionWrapper oneDark statusTitleEl <| statusEl m
        , if m.env.isLocalDevelopment && m.pref.logging then
            el [ width fill, alignBottom, height shrink ] <|
                map LoggerCtrl <|
                    Logger.historyEl m.log

          else
            none
        ]


sectionSpacingY : Int
sectionSpacingY =
    20


configSectionWrapper : ColorTheme -> Element Msg -> Element Msg -> Element Msg
configSectionWrapper theme titleEl element =
    column
        [ width fill
        , padding rectElementOuterPadding
        , spacing spacingUnit
        , BG.color theme.main
        , BD.rounded rectElementRound
        , Font.size sectionBaseFontSize
        ]
        [ el
            [ width fill
            , padding rectElementInnerPadding
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


slackConfigTitleEl : Element Msg
slackConfigTitleEl =
    row [ spacing spacingUnit ]
        [ slackLogoClippedEl [] sectionTitleFontSize
        , text "Slack"
        ]


discordConfigTitleEl : Element Msg
discordConfigTitleEl =
    row [ spacing spacingUnit ]
        [ squareIconOrHeadEl []
            { size = sectionTitleFontSize
            , name = "Discord"
            , url = Just (Discord.defaultIconUrl (Just sectionTitleFontSize))
            }
        , text "Discord"
        ]


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
            ColumnStore.size m.columnStore

        numVisible =
            Array.length m.columnStore.order
    in
    column [ padding rectElementInnerPadding, spacing spacingUnit, Font.size descFontSize ] <|
        List.map (row [ spacing spacingUnit ] << List.map text << List.intersperse "-")
            [ [ "Local message buffer capacity", StringExtra.punctuateNumber <| Broker.capacity m.itemBroker ]
            , [ "Maximum messages per column", StringExtra.punctuateNumber Column.columnItemLimit ]
            , [ "Number of columns", StringExtra.punctuateNumber numColumns ]
            , [ "* Visible columns", StringExtra.punctuateNumber numVisible ]
            , [ "* Pinned columns", StringExtra.punctuateNumber <| ColumnStore.sizePinned m.columnStore ]
            , [ "* Shadow columns", StringExtra.punctuateNumber (numColumns - numVisible) ]
            , [ "ClientHeight", StringExtra.punctuateNumber m.env.clientHeight ]
            , [ "ClientWidth", StringExtra.punctuateNumber m.env.clientWidth ]
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


prefTitleEl : Element Msg
prefTitleEl =
    row [ spacing spacingUnit ]
        [ octiconEl []
            { size = sectionTitleFontSize
            , color = oneDark.text
            , shape = Octicons.settings
            }
        , text "Preference"
        ]


prefEl : Pref -> ColumnStore -> Element Msg
prefEl pref columnStore =
    column [ width fill, padding rectElementInnerPadding, spacing (spacingUnit * 2) ]
        [ prefRowEl "Zephyr Mode" [ "When enabled, columns are automatically dismissed by LRU (least-recently-updated) manner." ] <|
            textColumn [ width fill, spacing spacingUnit, alignTop ]
                [ toggleInputEl []
                    { onChange = PrefCtrl << Pref.ZephyrMode
                    , height = sectionBaseFontSize
                    , checked = pref.zephyrMode
                    }
                , paragraph [] [ text ("Max columns: " ++ String.fromInt pref.evictThreshold) ]
                , descriptionEl
                    [ text <|
                        "Automatically calculated based on your screen width. "
                            ++ "If you pinned columns more than this limit, shadow columns do not automatically reappear."
                    ]
                ]
        , prefRowEl "Shadow Columns" [ "Columns currently aren't displayed. Automatically reappear when new messages arrived." ] <|
            let
                slotsAvailable =
                    not pref.zephyrMode || ColumnStore.sizePinned columnStore < pref.evictThreshold
            in
            shadowColumnsEl columnStore.fam slotsAvailable <| ColumnStore.listShadow columnStore
        ]


prefRowEl : String -> List String -> Element Msg -> Element Msg
prefRowEl title descriptions contents =
    row [ width fill, spacing spacingUnit ]
        [ textColumn [ width fill, spacing spacingUnit, alignTop ]
            [ text title
            , descriptionEl <| List.map text descriptions
            ]
        , contents
        ]


descriptionEl : List (Element Msg) -> Element Msg
descriptionEl texts =
    paragraph [ Font.size descFontSize, Font.color oneDark.note ] texts


descFontSize : Int
descFontSize =
    scale12 1


shadowColumnsEl : FilterAtomMaterial -> Bool -> List Column.Column -> Element Msg
shadowColumnsEl fam slotsAvailable shadowColumns =
    Element.Keyed.column [ width fill, spacing spacingUnit, alignTop ] <|
        case shadowColumns of
            [] ->
                [ ( "shadowColumnEmpty", descriptionEl [ text "(Empty)" ] ) ]

            _ ->
                List.map (shadowColumnKeyEl fam slotsAvailable) shadowColumns


shadowColumnKeyEl : FilterAtomMaterial -> Bool -> Column.Column -> ( String, Element Msg )
shadowColumnKeyEl fam slotsAvailable c =
    let
        theme =
            filtersToTheme c.filters
    in
    Tuple.pair c.id <|
        row [ width fill, spacing spacingUnit ]
            [ filtersToIconEl [] { size = shadowColumnIconSize, fam = fam, filters = c.filters }
            , filtersToTextEl [ Font.size descFontSize, Font.color oneDark.note ]
                { fontSize = descFontSize, color = oneDark.text, fam = fam, filters = c.filters }
            , showColumnButtonEl theme slotsAvailable c.id
            , deleteColumnButtonEl c.id
            ]


shadowColumnIconSize : Int
shadowColumnIconSize =
    descFontSize + iconSizeCompensation


iconSizeCompensation : Int
iconSizeCompensation =
    4


showColumnButtonEl : ColorTheme -> Bool -> String -> Element Msg
showColumnButtonEl theme slotsAvailable cId =
    thinButtonEl [ alignRight ]
        { onPress = ShowColumn cId
        , width = px showColumnButtonWidth
        , enabledColor = theme.prim
        , enabledFontColor = theme.text
        , enabled = slotsAvailable
        , innerElement =
            row [ Font.size descFontSize, spacing spacingUnit ]
                [ octiconEl [] { size = descFontSize, color = theme.text, shape = Octicons.arrowRight }
                , text "Show"
                ]
        }


showColumnButtonWidth : Int
showColumnButtonWidth =
    70


deleteColumnButtonEl : String -> Element Msg
deleteColumnButtonEl cId =
    squareButtonEl [ alignRight ]
        { onPress = DelColumn cId
        , enabled = True
        , round = 0
        , innerElement = octiconEl [] { size = shadowColumnIconSize, color = oneDark.err, shape = Octicons.trashcan }
        , innerElementSize = shadowColumnIconSize
        }
