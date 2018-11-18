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
import Html.Attributes exposing (style)
import Octicons
import View.ConfigPane exposing (configPaneEl)
import View.Parts exposing (..)


sidebarEl : Model -> Element Msg
sidebarEl ({ columnStore, viewState, env } as m) =
    column
        [ width (px (buttonSize + paddingX * 2))
        , height (fill |> maximum env.clientHeight)
        , alignLeft
        , paddingXY paddingX sectionSpacingY
        , onRight (configPaneEl m)
        , BG.color oneDark.bg
        ]
        [ el [ width fill, alignTop ] (columnButtonsEl columnStore)
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


columnButtonsEl : ColumnStore -> Element Msg
columnButtonsEl columnStore =
    (columnAddButtonKeyEl :: ColumnStore.mapForView columnButtonKeyEl columnStore)
        |> Element.Keyed.column
            [ width fill
            , paddingXY 0 sectionSpacingY
            , spacingXY 0 sectionSpacingY
            , Font.color oneDark.text
            ]


columnButtonKeyEl : FilterAtomMaterial -> Int -> Column.Column -> ( String, Element Msg )
columnButtonKeyEl fam index { id, filters, pinned } =
    Element.Input.button [ width (px buttonSize), height (px buttonSize) ]
        { onPress = Just (RevealColumn index)
        , label =
            filtersToIconEl [ inFront (pinBadgeEl pinned) ]
                { size = buttonSize, fam = fam, filters = filters }
        }
        |> Tuple.pair ("sidebarButton_" ++ id)


pinBadgeEl : Bool -> Element Msg
pinBadgeEl pinned =
    octiconEl
        [ alignTop
        , alignRight
        , visible pinned
        , htmlAttribute (style "transform" "rotate(-45deg)")
        ]
        { size = buttonSize // 3
        , color = columnPinColor
        , shape = Octicons.pin
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
        { onPress = Just AddEmptyColumn
        , label =
            octiconEl [ centerX, centerY ] { size = buttonSize // 2, color = defaultOcticonColor, shape = Octicons.plus }
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
                BG.color configButtonActiveBackground

              else
                mouseOver [ BG.color configButtonActiveBackground ]
            ]
            { onPress = Just (ToggleConfig (not configOpen))
            , label =
                octiconEl [ centerX, centerY ] { size = otherButtonSize, color = defaultOcticonColor, shape = Octicons.gear }
            }
        , newTabLink
            [ width (px buttonSize)
            , height (px buttonSize)
            , BD.rounded rectElementRound
            , BG.color sourceCodeLinkBackground
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , label =
                octiconEl [ centerX, centerY ] { size = otherButtonSize, color = defaultOcticonColor, shape = Octicons.markGithub }
            }
        ]


configButtonActiveBackground : Color
configButtonActiveBackground =
    oneDark.main


sourceCodeLinkBackground : Color
sourceCodeLinkBackground =
    oneDark.sub


otherButtonSize : Int
otherButtonSize =
    round (toFloat buttonSize * 0.667)
