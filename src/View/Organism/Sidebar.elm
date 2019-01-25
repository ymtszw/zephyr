module View.Organism.Sidebar exposing
    ( Props, ColumnProps, ColumnButton(..), Effects, render
    , styles, sidebarWidth
    )

{-| Sidebar Organism.

@docs Props, ColumnProps, ColumnButton, Props, Effects, render
@docs styles, sidebarWidth

-}

import Color exposing (cssRgba)
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Html, button, div, img, nav)
import Html.Attributes exposing (alt, class, src)
import Html.Events exposing (onClick)
import Html.Keyed
import Octicons
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image as Image
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (oneDark, oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Style exposing (..)


type alias Props =
    { configOpen : Bool
    , columns : List ( ColumnProps, ColumnButton )
    }


type alias Effects msg =
    { configOpener : msg
    , columnAdder : msg
    , columnButtonClickerByIndex : Int -> msg
    }


render : Effects msg -> Props -> Html msg
render eff p =
    nav
        [ class sidebarClass
        , flexColumn
        , spacingColumn15
        , oneDark
        , Background.colorBg
        , if p.configOpen then
            class configOpenClass

          else
            noAttr
        ]
        [ withTooltip (t "Add Column") <| addColumnButton eff.columnAdder
        , columnButtons eff.columnButtonClickerByIndex p.columns
        , otherButtons eff.configOpener
        ]


columnButtons : (Int -> msg) -> List ( ColumnProps, ColumnButton ) -> Html msg
columnButtons columnButtonClickerByIndex columns =
    Html.Keyed.node "div" [ class columnButtonsClass, flexColumn, flexGrow, flexBasisAuto, spacingColumn10 ] <|
        List.indexedMap (colummButtonKey columnButtonClickerByIndex) columns


addColumnButton : msg -> Html msg
addColumnButton columnAdder =
    Icon.octiconButton
        [ class buttonClass
        , class octiconButtonClass
        , flexItem
        , padding5
        , Border.round5
        , Border.dashed
        , Border.w1
        ]
        { onPress = columnAdder
        , size = octiconSize - 2 -- Subtract border width
        , shape = Octicons.plus
        }


withTooltip : Html msg -> Html msg -> Html msg
withTooltip tooltip content =
    div []
        [ div [ class sidebarTooltipClass ] [ tooltip ]
        , content
        ]


type ColumnButton
    = Fallback String
    | DiscordButton { channelName : String, guildIcon : Maybe String }
    | SlackButton { convName : String, teamIcon : Maybe String }


type alias ColumnProps =
    { id : String, pinned : Bool }


colummButtonKey : (Int -> msg) -> Int -> ( ColumnProps, ColumnButton ) -> ( String, Html msg )
colummButtonKey columnButtonClicker index ( cp, cb ) =
    ( "columnButton_" ++ cp.id
    , button
        [ class buttonClass
        , flexItem
        , noPadding
        , Border.round5
        , onClick (columnButtonClicker index)
        ]
        [ columnButtonFace cp.pinned cb
        ]
    )


columnButtonFace : Bool -> ColumnButton -> Html msg
columnButtonFace pinned cb =
    withPin pinned <|
        case cb of
            Fallback desc ->
                ( Nothing, Icon.abbr [ class innerFaceClass, serif, sizeTitle, Border.round5 ] desc )

            DiscordButton opts ->
                ( Just discordBadge
                , Icon.imgOrAbbr [ class innerFaceClass, serif, sizeTitle, Border.round5 ] opts.channelName opts.guildIcon
                )

            SlackButton opts ->
                ( Just slackBadge
                , Icon.imgOrAbbr [ class innerFaceClass, serif, sizeTitle, Border.round5 ] opts.convName opts.teamIcon
                )


withPin : Bool -> ( Maybe (Html msg), Html msg ) -> Html msg
withPin pinned ( bottomRight, content ) =
    withBadge []
        { topRight =
            if pinned then
                Just pinBadge

            else
                Nothing
        , bottomRight = bottomRight
        , content = div [ padding2, growColumn ] [ content ] -- Insetting by 2px
        }


pinBadge : Html msg
pinBadge =
    div [ class badgeClass ] [ Image.octicon { size = badgeSize, shape = Octicons.pin } ]


discordBadge : Html msg
discordBadge =
    imageBadge "Discord logo" <| Discord.defaultIconUrl (Just badgeSize)


imageBadge : String -> String -> Html msg
imageBadge alt_ src_ =
    img [ class badgeClass, Border.round2, src src_, alt alt_ ] []


slackBadge : Html msg
slackBadge =
    imageBadge "Slack logo" <| Slack.defaultIconUrl (Just badgeSize)


otherButtons : msg -> Html msg
otherButtons configOpener =
    div [ flexColumn, flexBasisAuto, spacingColumn10 ]
        [ Icon.octiconButton
            [ class buttonClass
            , class octiconButtonClass
            , class configToggleButtonClass
            , flexItem
            , padding5
            , Border.round5
            ]
            { onPress = configOpener
            , size = octiconSize
            , shape = Octicons.gear
            }
        , Icon.octiconLink
            [ newTab
            , class buttonClass
            , class octiconButtonClass
            , flexItem
            , padding5
            , Border.round5
            ]
            { url = "https://github.com/ymtszw/zephyr"
            , size = octiconSize
            , shape = Octicons.markGithub
            }
        ]



-- STYLES


styles : List Style
styles =
    [ s (c sidebarClass)
        [ ( "position", "fixed" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "width", px sidebarWidth )
        , ( "height", "100vh" )
        , ( "padding", px paddingY ++ " " ++ px paddingX )
        ]
    , s (c sidebarClass ++ ":hover," ++ c sidebarClass ++ c configOpenClass)
        [ ( "width", px (sidebarWidth + sidebarExpansionWidth) )
        ]
    , s (c sidebarTooltipClass)
        [ ( "position", "absolute" )
        , ( "left", px (buttonSize + paddingX) )
        , ( "width", px (sidebarExpansionWidth - paddingX) )
        , ( "max-width", px (sidebarExpansionWidth - paddingX) )
        , ( "height", px buttonSize )
        , ( "max-height", px buttonSize )
        , ( "overflow", "hidden" )
        , ( "opacity", "0" )
        , ( "visibility", "hidden" )
        ]
    , s
        (String.join ","
            [ c sidebarClass ++ ":hover " ++ c sidebarTooltipClass
            , c sidebarClass ++ c configOpenClass ++ " " ++ c sidebarTooltipClass
            ]
        )
        [ ( "opacity", "1" )
        , ( "visibility", "visible" )
        ]
    , s (c sidebarClass ++ " " ++ c columnButtonsClass)
        [ ( "max-height", "calc(100vh - " ++ px (3 * buttonSize + 2 * paddingY + 2 * 15 + 10) ++ ")" )
        , ( "overflow-y", "auto" )
        ]
    , s (c sidebarClass ++ " " ++ c buttonClass)
        [ ( "width", px buttonSize )
        , ( "height", px buttonSize )
        , ( "flex-basis", "auto" )
        , ( "background-color", "inherit" )
        ]
    , s (c sidebarClass ++ " " ++ c badgeClass)
        [ ( "width", px badgeSize )
        , ( "height", px badgeSize )
        , ( "overflow", "hidden" )
        ]
    , s (c sidebarClass ++ " " ++ c badgeClass ++ " " ++ c "pin")
        [ ( "transform", "rotate(-45deg)" )
        ]
    , s (c sidebarClass ++ " " ++ c innerFaceClass)
        [ ( "width", px innerFaceSize )
        , ( "height", px innerFaceSize )
        ]
    , s (c sidebarClass ++ " " ++ c octiconButtonClass)
        [ ( "background-color", cssRgba oneDarkTheme.bg ) ]
    , hov (c sidebarClass ++ " " ++ c octiconButtonClass)
        [ ( "background-color", cssRgba oneDarkTheme.sub ) ]
    , s (c sidebarClass ++ c configOpenClass ++ " " ++ c configToggleButtonClass)
        [ ( "background-color", cssRgba oneDarkTheme.sub ) ]
    , s (c sidebarClass ++ " " ++ c badgeClass ++ " " ++ c "pin" ++ " path")
        [ ( "fill", cssRgba oneDarkTheme.warn ) ]
    , Image.octiconPathStyle (c sidebarClass ++ c configOpenClass ++ " " ++ c configToggleButtonClass)
        [ ( "fill", cssRgba oneDarkTheme.text ) ]
    ]


sidebarClass : String
sidebarClass =
    "sbar"


sidebarWidth : Int
sidebarWidth =
    buttonSize + paddingX * 2


sidebarExpansionWidth : Int
sidebarExpansionWidth =
    150


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


buttonClass : String
buttonClass =
    "sbarbtn"


buttonSize : Int
buttonSize =
    40


badgeClass : String
badgeClass =
    "sbarbadge"


badgeSize : Int
badgeSize =
    12


innerFaceClass : String
innerFaceClass =
    "sbarcbtninner"


innerFaceSize : Int
innerFaceSize =
    buttonSize - (2 * 2)


octiconButtonClass : String
octiconButtonClass =
    "sbaroctbtn"


octiconSize : Int
octiconSize =
    buttonSize - (paddingX * 2)


configToggleButtonClass : String
configToggleButtonClass =
    "sbarctoggle"
