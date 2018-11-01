module View.Sidebar exposing (sidebarEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer as Producer
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Octicons
import View.Parts exposing (octiconEl)


sidebarEl : Model -> Element Msg
sidebarEl { columnStore, viewState, env } =
    column
        [ width (px 50)
        , height (fill |> maximum env.clientHeight)
        , alignLeft
        , paddingXY 0 10
        , BG.color oneDark.bg
        ]
        [ el [ width fill, alignTop ] (columnButtonsEl columnStore)
        , el [ width fill, alignBottom ] (lazy otherButtonsEl viewState.configOpen)
        ]


columnButtonsEl : ColumnStore -> Element Msg
columnButtonsEl columnStore =
    Element.Keyed.column [ width fill, padding 5, spacingXY 0 10 ] <|
        (columnAddButtonKeyEl :: ColumnStore.indexedMap columnButtonKeyEl columnStore)


columnButtonKeyEl : Int -> Column.Column -> ( String, Element Msg )
columnButtonKeyEl index { id } =
    Element.Input.button
        [ width (px 40)
        , height (px 40)
        , clip
        , Font.color oneDark.note
        , BD.width 1
        , BD.color oneDark.note
        , BD.rounded 10
        ]
        { onPress = Just (DelColumn index), label = el [ centerX, centerY ] <| text "×" }
        |> el [ width fill ]
        |> Tuple.pair ("sidebarButton_" ++ id)


columnAddButtonKeyEl : ( String, Element Msg )
columnAddButtonKeyEl =
    Element.Input.button
        [ width (px 40)
        , height (px 40)
        , clip
        , Font.color oneDark.note
        , BD.dashed
        , BD.width 1
        , BD.color oneDark.note
        , BD.rounded 10
        ]
        { onPress = Just AddColumn, label = el [ centerX, centerY ] <| text "+" }
        |> el [ width fill ]
        |> Tuple.pair "columnAddButton"


otherButtonsEl : Bool -> Element Msg
otherButtonsEl configOpen =
    column [ width fill, padding 5, spacingXY 0 10 ]
        [ Element.Input.button
            [ width (px 40)
            , height (px 40)
            , BD.rounded 10
            , if configOpen then
                BG.color oneDark.main

              else
                mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not configOpen))
            , label = el [ centerX, centerY ] <| octiconEl Octicons.gear
            }
        , newTabLink
            [ width (px 40)
            , height (px 40)
            , BD.rounded 10
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = el [ centerX, centerY ] <| octiconEl Octicons.markGithub
            }
        ]
