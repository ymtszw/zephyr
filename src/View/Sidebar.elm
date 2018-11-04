module View.Sidebar exposing (sidebarEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter exposing (Filter, FilterAtom(..))
import Data.FilterAtomMaterial exposing (FilterAtomMaterial)
import Data.Model as Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Octicons
import View.Parts exposing (..)


sidebarEl : Model -> Element Msg
sidebarEl { columnStore, viewState, env } =
    column
        [ width (px (buttonSize + paddingX * 2))
        , height (fill |> maximum env.clientHeight)
        , alignLeft
        , paddingXY paddingX sectionSpacingY
        , BG.color oneDark.bg
        ]
        [ el [ width fill, alignTop ] (columnButtonsEl viewState.filterAtomMaterial columnStore)
        , el [ width fill, alignBottom ] (lazy otherButtonsEl viewState.configOpen)
        ]


buttonSize : Int
buttonSize =
    40


paddingX : Int
paddingX =
    rectElementInnerPadding


sectionSpacingY : Int
sectionSpacingY =
    10


columnButtonsEl : FilterAtomMaterial -> ColumnStore -> Element Msg
columnButtonsEl fam columnStore =
    (columnAddButtonKeyEl :: ColumnStore.indexedMap (columnButtonKeyEl fam) columnStore)
        |> Element.Keyed.column
            [ width fill
            , paddingXY 0 sectionSpacingY
            , spacingXY 0 sectionSpacingY
            , Font.color oneDark.text
            ]


columnButtonKeyEl : FilterAtomMaterial -> Int -> Column.Column -> ( String, Element Msg )
columnButtonKeyEl fam index { id, filters } =
    filtersToIconEl buttonSize fam filters
        |> asColumnButton index id
        |> Tuple.pair ("sidebarButton_" ++ id)


asColumnButton : Int -> String -> Element Msg -> Element Msg
asColumnButton index cId element =
    Element.Input.button [ width (px buttonSize), height (px buttonSize) ]
        { onPress = Just (RevealColumn index)
        , label = element
        }


columnAddButtonKeyEl : ( String, Element Msg )
columnAddButtonKeyEl =
    Element.Input.button
        [ width (px buttonSize)
        , height (px buttonSize)
        , clip
        , Font.color oneDark.note
        , BD.dashed
        , BD.width 1
        , BD.color oneDark.note
        , BD.rounded rectElementRound
        ]
        { onPress = Just AddColumn
        , label =
            el [ centerX, centerY ] <|
                octiconFreeSizeEl (buttonSize // 2) Octicons.plus
        }
        |> el [ width fill ]
        |> Tuple.pair "columnAddButton"


otherButtonsEl : Bool -> Element Msg
otherButtonsEl configOpen =
    column [ width fill, paddingXY 0 sectionSpacingY, spacingXY 0 sectionSpacingY ]
        [ Element.Input.button
            [ width (px buttonSize)
            , height (px buttonSize)
            , BD.rounded rectElementRound
            , if configOpen then
                BG.color oneDark.main

              else
                mouseOver [ BG.color oneDark.main ]
            ]
            { onPress = Just (ToggleConfig (not configOpen))
            , label = el [ centerX, centerY ] <| octiconEl Octicons.gear
            }
        , newTabLink
            [ width (px buttonSize)
            , height (px buttonSize)
            , BD.rounded rectElementRound
            , BG.color oneDark.sub
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label = el [ centerX, centerY ] <| octiconEl Octicons.markGithub
            }
        ]
