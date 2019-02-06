module View.Organisms.Sidebar exposing
    ( Props, ColumnInSidebar, Effects, render
    , styles, sidebarWidth, sidebarExpansionWidth
    )

{-| Sidebar Organism.

@docs Props, ColumnInSidebar, Effects, render
@docs styles, sidebarWidth, sidebarExpansionWidth

-}

import Html exposing (Html, button, div, nav, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Keyed
import Octicons
import View.Atoms.Background as Background
import View.Atoms.Border as Border
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Theme exposing (oneDark)
import View.Atoms.Typography exposing (..)
import View.Molecules.Column as Column exposing (ColumnProps)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias Props c =
    { configOpen : Bool
    , visibleColumns : List (ColumnInSidebar c)
    }


type alias ColumnInSidebar c =
    ColumnProps { c | id : String }


type alias Effects msg =
    { configOpener : msg
    , columnAdder : msg
    , columnButtonClickerByIndex : Int -> msg
    }


render : Effects msg -> Props c -> Html msg
render eff p =
    nav
        [ class sidebarClass
        , flexColumn
        , flexCenter
        , spacingColumn15
        , oneDark
        , Background.colorBg
        , if p.configOpen then
            class configOpenClass

          else
            noAttr
        ]
        [ withTooltip (span [ colorNote ] [ t "Add Column" ]) <| addColumnButton eff.columnAdder
        , columnButtons eff.columnButtonClickerByIndex p.visibleColumns
        , otherButtons eff.configOpener p.configOpen
        ]


columnButtons : (Int -> msg) -> List (ColumnInSidebar c) -> Html msg
columnButtons columnButtonClickerByIndex visibleColumns =
    Html.Keyed.node "div" [ class columnButtonsClass, flexColumn, flexGrow, flexBasisAuto, padding5, spacingColumn10 ] <|
        List.indexedMap (colummButtonKey columnButtonClickerByIndex) visibleColumns


addColumnButton : msg -> Html msg
addColumnButton columnAdder =
    Icon.octiconButton
        [ flexItem
        , flexBasisAuto
        , padding5
        , Border.round5
        , Border.dashed
        , Border.w1
        , Icon.rounded40
        , Background.transparent
        , Background.hovSub
        ]
        { onPress = columnAdder
        , size = octiconSize - 2 -- Subtract border width
        , shape = Octicons.plus
        }


withTooltip : Html msg -> Html msg -> Html msg
withTooltip tooltip content =
    div [ flexRow, flexBasisAuto, spacingRow5 ]
        [ content
        , div
            [ class sidebarTooltipClass
            , flexRow
            , flexCenter
            , flexBasisAuto
            , padding5
            , sizeHeadline
            , Background.colorSub
            , Border.round5
            ]
            [ tooltip ]
        ]


colummButtonKey : (Int -> msg) -> Int -> ColumnInSidebar c -> ( String, Html msg )
colummButtonKey columnButtonClicker index c =
    Tuple.pair c.id <|
        withTooltip (Column.blockTitle [ flexBasisAuto ] c) <|
            button
                [ flexItem
                , noPadding
                , onClick (columnButtonClicker index)
                , Icon.rounded40
                , Background.transparent
                ]
                [ Column.icon40 c
                ]


otherButtons : msg -> Bool -> Html msg
otherButtons configOpener configOpen =
    let
        note x =
            span [ bold, colorNote ] [ t x ]
    in
    div [ flexColumn, flexBasisAuto, spacingColumn10 ]
        [ withTooltip (note "Zephyr Config") <|
            let
                baseAttrs =
                    [ flexItem
                    , flexBasisAuto
                    , padding5
                    , Icon.rounded40
                    ]

                statefulAttrs =
                    if configOpen then
                        [ Image.fillText, Background.colorSub ]

                    else
                        [ Background.hovSub
                        , Background.transparent
                        ]
            in
            Icon.octiconButton (baseAttrs ++ statefulAttrs)
                { onPress = configOpener
                , size = octiconSize
                , shape = Octicons.gear
                }
        , withTooltip (note "Source") <|
            Icon.octiconLink
                [ newTab
                , flexItem
                , flexBasisAuto
                , padding5
                , Icon.rounded40
                , Background.transparent
                , Background.hovSub
                ]
                { url = "https://github.com/ymtszw/zephyr"
                , size = octiconSize
                , shape = Octicons.markGithub
                }
        ]



-- STYLES


styles : List Style
styles =
    let
        openedSidebars =
            [ hov (c sidebarClass)
            , c sidebarClass ++ c configOpenClass
            ]
    in
    [ s (c sidebarClass)
        [ ( "position", "fixed" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "width", px sidebarWidth )
        , ( "height", "100vh" )
        , ( "padding-top", px paddingY )
        , ( "padding-bottom", px paddingY )
        ]
    , s (String.join "," openedSidebars)
        [ ( "width", px (sidebarWidth + sidebarExpansionWidth) )
        ]
    , s (c sidebarTooltipClass)
        [ ( "width", px (sidebarExpansionWidth - paddingX) )
        , ( "max-width", px (sidebarExpansionWidth - paddingX) )
        , ( "height", px buttonSize )
        , ( "max-height", px buttonSize )
        , ( "display", "none" )
        ]
    , s (String.join "," (List.map (\anc -> descOf anc (c sidebarTooltipClass)) openedSidebars))
        [ ( "display", "flex" ) ]
    , s (c columnButtonsClass)
        [ ( "max-height", "calc(100vh - " ++ px (3 * buttonSize + 2 * paddingY + 2 * 15 + 10) ++ ")" )
        , ( "overflow-y", "auto" )
        ]
    ]


sidebarClass : String
sidebarClass =
    "sbar"


sidebarWidth : Int
sidebarWidth =
    buttonSize + paddingX * 2


sidebarExpansionWidth : Int
sidebarExpansionWidth =
    130


paddingX : Int
paddingX =
    5


paddingY : Int
paddingY =
    20


configOpenClass : String
configOpenClass =
    "sbarcopen"


sidebarTooltipClass : String
sidebarTooltipClass =
    "sbarttip"


columnButtonsClass : String
columnButtonsClass =
    "sbarcbtns"


buttonSize : Int
buttonSize =
    40


octiconSize : Int
octiconSize =
    buttonSize - (paddingX * 2)
