module View.Organism.Sidebar exposing (ColumnButton(..), ColumnProps, Props, sidebar, styles)

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
import View.Atom.Theme exposing (oneDarkTheme)
import View.Atom.Typography exposing (..)
import View.Molecule.Icon as Icon
import View.Style exposing (..)


type alias Props msg =
    { configOpen : Bool
    , configOpener : msg
    , columnAdder : msg
    , columnButtonClicker : Int -> msg
    , columns : List ( ColumnProps, ColumnButton )
    }


sidebar : Props msg -> Html msg
sidebar p =
    nav
        [ class sidebarClass
        , flexColumn
        , spacingColumn15
        , Background.colorBg
        ]
        [ addColumnButton p.columnAdder
        , columnButtons p
        , otherButtons p.configOpener p.configOpen
        ]


columnButtons : Props msg -> Html msg
columnButtons p =
    Html.Keyed.node "div" [ class columnButtonsClass, flexColumn, flexGrow, flexBasisAuto, spacingColumn10 ] <|
        List.indexedMap (colummButtonKey p.columnButtonClicker) p.columns


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
                ( Nothing, abbrIcon desc )

            DiscordButton opts ->
                Tuple.pair (Just discordBadge) <|
                    case opts.guildIcon of
                        Just src ->
                            imageIcon src "discord guild icon"

                        Nothing ->
                            abbrIcon opts.channelName

            SlackButton opts ->
                Tuple.pair (Just slackBadge) <|
                    case opts.teamIcon of
                        Just src ->
                            imageIcon src "slack team icon"

                        Nothing ->
                            abbrIcon opts.convName


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


abbrIcon : String -> Html msg
abbrIcon desc =
    div
        [ class innerFaceClass
        , flexColumn
        , flexCenter
        , serif
        , sizeTitle
        , Border.round5
        , Border.solid
        , Border.w1
        ]
        [ div [] [ t (String.left 1 desc) ]
        ]


imageIcon : String -> String -> Html msg
imageIcon src_ alt_ =
    img
        [ class innerFaceClass
        , Border.round5
        , src src_
        , alt alt_
        ]
        []


discordBadge : Html msg
discordBadge =
    img [ class badgeClass, Border.round2, src discordBadgeUrl, alt "discord logo" ] []


discordBadgeUrl : String
discordBadgeUrl =
    Discord.defaultIconUrl (Just badgeSize)


slackBadge : Html msg
slackBadge =
    div [ class badgeClass, Border.round2 ]
        [ img [ src slackBadgeUrl, alt "slack logo", class slackBadgeClass ] []
        ]


slackBadgeUrl : String
slackBadgeUrl =
    Slack.defaultIconUrl (Just slackBadgeSize)


otherButtons : msg -> Bool -> Html msg
otherButtons configOpener configOpen =
    div [ flexColumn, flexBasisAuto, spacingColumn10 ]
        [ Icon.octiconButton
            [ class buttonClass
            , class octiconButtonClass
            , flexItem
            , padding5
            , Border.round5
            , if configOpen then
                class configOpenClass

              else
                noAttr
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
    , s (c sidebarClass ++ " " ++ c badgeClass ++ " " ++ c slackBadgeClass)
        [ ( "width", px slackBadgeSize )
        , ( "height", px slackBadgeSize )
        , ( "transform", "translate(" ++ slackBadgeTranslate ++ "px," ++ slackBadgeTranslate ++ "px)" )
        ]
    , s (c sidebarClass ++ " " ++ c badgeClass ++ " " ++ c "pin")
        [ ( "transform", "rotate(-45deg)" )
        ]
    , s (c sidebarClass ++ " " ++ c innerFaceClass)
        [ ( "width", px innerFaceSize )
        , ( "height", px innerFaceSize )
        , ( "justify-content", "center" )
        ]
    , s (c sidebarClass ++ " " ++ c octiconButtonClass)
        [ ( "background-color", cssRgba oneDarkTheme.bg ) ]
    , s (c sidebarClass ++ " " ++ c octiconButtonClass ++ ":hover")
        [ ( "background-color", cssRgba oneDarkTheme.sub ) ]
    , s (c sidebarClass ++ " " ++ c octiconButtonClass ++ c configOpenClass)
        [ ( "background-color", cssRgba oneDarkTheme.sub ) ]
    , s (c sidebarClass ++ " " ++ c badgeClass ++ " " ++ c "pin" ++ " path")
        [ ( "fill", cssRgba oneDarkTheme.warn ) ]
    , Image.octiconPathStyle (c sidebarClass ++ " " ++ c octiconButtonClass ++ c configOpenClass)
        [ ( "fill", cssRgba oneDarkTheme.text ) ]
    ]


sidebarClass : String
sidebarClass =
    "sbar"


sidebarWidth : Int
sidebarWidth =
    buttonSize + paddingX * 2


paddingX : Int
paddingX =
    5


paddingY : Int
paddingY =
    20


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


slackBadgeClass : String
slackBadgeClass =
    "sbarslackbadge"


slackBadgeSize : Int
slackBadgeSize =
    (badgeSize * 7) // 5


slackBadgeTranslate : String
slackBadgeTranslate =
    String.fromFloat (toFloat (badgeSize - slackBadgeSize) / 2)


octiconButtonClass : String
octiconButtonClass =
    "sbaroctbtn"


octiconSize : Int
octiconSize =
    buttonSize - (paddingX * 2)


configOpenClass : String
configOpenClass =
    "sbarcopen"
