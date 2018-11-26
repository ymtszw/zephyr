module View.ConfigPane exposing (configPaneEl)

import Array
import Broker
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Pref as Pref exposing (Pref)
import Data.Producer as Producer exposing (ProducerRegistry)
import Data.Producer.Discord as Discord
import Deque
import Dict
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Keyed
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
        [ configSectionWrapper prefTitleEl <|
            prefEl m.pref m.columnStore
        , configSectionWrapper discordConfigTitleEl <|
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
        [ row [ width fill, spacing spacingUnit ]
            [ textColumn [ width fill, spacing spacingUnit, alignTop ]
                [ text "Zephyr Mode"
                , description
                    [ text "When enabled, columns are automatically dismissed by LRU (least-recently-updated) manner. Also, columns with new messages will automatically reappear."
                    ]
                ]
            , textColumn [ width fill, spacing spacingUnit, alignTop ]
                [ toggleInputEl []
                    { onChange = ZephyrMode
                    , height = sectionBaseFontSize
                    , checked = pref.zephyrMode
                    }
                , paragraph [] [ text ("Max columns: " ++ String.fromInt pref.evictThreshold) ]
                , description [ text "Automatically calculated based on your screen width." ]
                ]
            ]
        , row [ width fill, spacing spacingUnit ]
            [ textColumn [ width fill, spacing spacingUnit, alignTop ]
                [ text "Shadow Columns"
                , description [ text "Currently not displayed columns." ]
                ]
            , shadowColumnsEl columnStore.fam <|
                ColumnStore.listShadow columnStore
            ]
        ]


description : List (Element Msg) -> Element Msg
description texts =
    paragraph [ Font.size descFontSize, Font.color oneDark.note ] texts


descFontSize : Int
descFontSize =
    scale12 1


shadowColumnsEl : FilterAtomMaterial -> List Column.Column -> Element Msg
shadowColumnsEl fam shadowColumns =
    Element.Keyed.column [ width fill, spacing spacingUnit, alignTop ] <|
        case shadowColumns of
            [] ->
                [ ( "shadowColumnEmpty", description [ text "(Empty)" ] ) ]

            _ ->
                List.map (shadowColumnKeyEl fam) shadowColumns


shadowColumnKeyEl : FilterAtomMaterial -> Column.Column -> ( String, Element Msg )
shadowColumnKeyEl fam c =
    Tuple.pair c.id <|
        row [ width fill, spacing spacingUnit ]
            [ filtersToIconEl [] { size = descFontSize, fam = fam, filters = c.filters }
            , filtersToTextEl [ Font.size descFontSize, Font.color oneDark.note ]
                { fontSize = descFontSize, color = oneDark.text, fam = fam, filters = c.filters }
            , showColumnButtonEl c.id
            ]


showColumnButtonEl : String -> Element Msg
showColumnButtonEl cId =
    thinButtonEl [ alignRight ]
        { onPress = NoOp
        , width = px showColumnButtonWidth
        , enabledColor = oneDark.prim
        , enabledFontColor = oneDark.text
        , enabled = True
        , innerElement =
            row [ Font.size descFontSize, spacing spacingUnit ]
                [ octiconEl [] { size = descFontSize, color = oneDark.text, shape = Octicons.arrowRight }
                , text "Show"
                ]
        }


showColumnButtonWidth : Int
showColumnButtonWidth =
    70


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
            Dict.size m.columnStore.dict

        numVisible =
            Array.length m.columnStore.order
    in
    column [ padding rectElementInnerPadding, spacing spacingUnit, Font.size descFontSize ] <|
        List.map (row [ spacing spacingUnit ] << List.map text << List.intersperse "-")
            [ [ "Local message buffer capacity", StringExtra.punctuateNumber <| Broker.capacity m.itemBroker ]
            , [ "Maximum messages per column", StringExtra.punctuateNumber Column.columnItemLimit ]
            , [ "Number of columns", StringExtra.punctuateNumber numColumns ]
            , [ "* Visible columns", StringExtra.punctuateNumber numVisible ]
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
