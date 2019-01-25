module View.Organism.Config.Pref exposing (Effects, Props, ShadowColumn(..), ShadowColumnProps, render, styles)

import Color exposing (cssRgba)
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Html, button, div, h3, img, p)
import Html.Attributes exposing (alt, class, disabled, src)
import Html.Events exposing (onClick)
import Html.Keyed
import Octicons
import View.Atom.Background as Background
import View.Atom.Border as Border
import View.Atom.Image exposing (octicon, octiconPathStyle)
import View.Atom.Input as Input
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (..)
import View.Atom.Typography exposing (..)
import View.ConfigPane.DiscordConfig as Discord
import View.Molecule.Icon as Icon
import View.Molecule.Table as Table
import View.Style exposing (..)


type alias Effects msg =
    { onZephyrModeChange : Bool -> msg
    , onShowColumnButtonClick : String -> msg
    , onDeleteColumnButtonClick : String -> msg
    , onLoggingChange : Bool -> msg
    }


type alias Props =
    { zephyrMode : Bool
    , evictThreshold : Int
    , columnSlotsAvailable : Bool
    , shadowColumns : List ( ShadowColumnProps, ShadowColumn )
    , logging : Bool
    }


render : Effects msg -> Props -> Html msg
render eff props =
    div [ flexColumn, padding5, spacingColumn10 ]
        [ prefRow "Zephyr Mode" [ "When enabled, columns are automatically dismissed by LRU (least-recently-updated) manner." ] <|
            div [ flexColumn, spacingColumn5 ]
                [ Input.toggle [] { onChange = eff.onZephyrModeChange, checked = props.zephyrMode }
                , p [] [ t ("Max columns: " ++ String.fromInt props.evictThreshold) ]
                , desc
                    [ t "Automatically calculated based on your screen width. "
                    , t "If you pinned columns more than this limit, shadow columns do not automatically reappear."
                    ]
                ]
        , prefRow "Shadow Columns" [ "Columns currently aren't displayed. Automatically reappear when new messages arrived." ] <|
            shadowColumnsTable eff props.columnSlotsAvailable props.shadowColumns
        , prefRow "Logging" [ "Enables Elm events inspector at the bottom of this pane. This will SIGNIFICANTLY degrade application performance!" ] <|
            div [] [ Input.toggle [] { onChange = eff.onLoggingChange, checked = props.logging } ]
        ]


prefRow : String -> List String -> Html msg -> Html msg
prefRow title descriptions contents =
    div [ growRow, spacingRow5 ]
        [ div [ flexColumn, spacingColumn5 ]
            [ h3 [ sizeTitle ] [ t title ]
            , desc (List.map t descriptions)
            ]
        , contents
        ]


desc : List (Html msg) -> Html msg
desc texts =
    p [ colorNote ] texts


type ShadowColumn
    = FallbackSC
    | DiscordSC { mainChannelName : String, guildIcon : Maybe String }
    | SlackSC { mainConvName : String, teamIcon : Maybe String }


type alias ShadowColumnProps =
    { id : String, description : String }


shadowColumnsTable : Effects msg -> Bool -> List ( ShadowColumnProps, ShadowColumn ) -> Html msg
shadowColumnsTable eff slotsAvailable shadowColumns =
    let
        columnCell ( scp, sc ) =
            ( [ widthFill, theme sc ]
            , [ div [ flexRow, flexCenter, spacingRow5 ]
                    [ shadowColumnIcon scp.description sc
                    , div [ bold ] [ t scp.description ]
                    ]
              ]
            )

        actionCell ( scp, sc ) =
            ( [ theme sc ]
            , [ div [ flexRow, flexCenter, spacingRow2 ]
                    [ showColumnButton (eff.onShowColumnButtonClick scp.id) slotsAvailable
                    , deleteColumnButton (eff.onDeleteColumnButtonClick scp.id)
                    ]
              ]
            )

        theme sc =
            case sc of
                SlackSC _ ->
                    aubergine

                _ ->
                    noAttr
    in
    Table.render []
        { columns = [ { header = "Column", cell = columnCell }, { header = "Action", cell = actionCell } ]
        , rowKey = \( scp, _ ) -> scp.id
        , data = shadowColumns
        }


shadowColumnIcon : String -> ShadowColumn -> Html msg
shadowColumnIcon description sc =
    case sc of
        FallbackSC ->
            Icon.abbr [ class shadowColumnIconClass, serif, Border.round2 ] description

        DiscordSC { mainChannelName, guildIcon } ->
            badgedIcon discordBadge <|
                Icon.imgOrAbbr [ class shadowColumnIconClass, serif, Border.round2 ] "Discord guild icon" guildIcon

        SlackSC { mainConvName, teamIcon } ->
            badgedIcon slackBadge <|
                Icon.imgOrAbbr [ class shadowColumnIconClass, serif, Border.round2 ] "Slack team icon" teamIcon


badgedIcon : Html msg -> Html msg -> Html msg
badgedIcon badge icon =
    withBadge []
        { topRight = Nothing
        , bottomRight = Just badge
        , content = icon -- No inset
        }


discordBadge : Html msg
discordBadge =
    imageBadge "Discord logo" <| Discord.defaultIconUrl (Just shadowColumnIconBadgeSize)


imageBadge : String -> String -> Html msg
imageBadge alt_ src_ =
    img [ class shadowColumnIconBadgeClass, src src_, alt alt_ ] []


slackBadge : Html msg
slackBadge =
    imageBadge "Slack logo" <| Slack.defaultIconUrl (Just shadowColumnIconBadgeSize)


showColumnButton : msg -> Bool -> Html msg
showColumnButton onShowColumnButtonClick slotsAvailable =
    button
        [ class showColumnButtonClass
        , flexItem
        , flexRow
        , flexCenter
        , padding2
        , Background.colorPrim
        , disabled (not slotsAvailable)
        , onClick onShowColumnButtonClick
        ]
        [ octicon { size = showColumnButtonOcticonSize, shape = Octicons.arrowRight }, t " Show" ]


deleteColumnButton : msg -> Html msg
deleteColumnButton onDeleteColumnButtonClick =
    Icon.octiconButton [ class deleteColumnButtonClass, flexItem, flexBasisAuto, Background.transparent ]
        { onPress = onDeleteColumnButtonClick
        , size = shadowColumnIconSize
        , shape = Octicons.trashcan
        }



-- STYLES


styles : List Style
styles =
    [ s (c shadowColumnIconClass)
        [ ( "width", px shadowColumnIconSize )
        , ( "height", px shadowColumnIconSize )
        ]
    , s (c shadowColumnIconBadgeClass)
        [ ( "width", px shadowColumnIconBadgeSize )
        , ( "height", px shadowColumnIconBadgeSize )
        ]
    , s (c showColumnButtonClass)
        [ ( "width", px showColumnButtonWidth )
        , ( "height", px shadowColumnIconSize )
        , ( "justify-content", "center" )
        , ( "flex-basis", "auto" )
        ]
    , octiconPathStyle (c oneDarkClass ++ " " ++ c showColumnButtonClass) [ ( "fill", cssRgba oneDarkTheme.text ) ]
    , octiconPathStyle (c aubergineClass ++ " " ++ c showColumnButtonClass) [ ( "fill", cssRgba aubergineTheme.text ) ]
    , s (c deleteColumnButtonClass)
        [ ( "width", px shadowColumnIconSize )
        , ( "height", px shadowColumnIconSize )
        ]
    , octiconPathStyle (c oneDarkClass ++ " " ++ c deleteColumnButtonClass) [ ( "fill", cssRgba oneDarkTheme.err ) ]
    , octiconPathStyle (c aubergineClass ++ " " ++ c deleteColumnButtonClass) [ ( "fill", cssRgba aubergineTheme.err ) ]
    ]


shadowColumnIconClass : String
shadowColumnIconClass =
    "scicon"


shadowColumnIconSize : Int
shadowColumnIconSize =
    -- Bigger than base font size
    20


shadowColumnIconBadgeClass : String
shadowColumnIconBadgeClass =
    "scbadge"


shadowColumnIconBadgeSize : Int
shadowColumnIconBadgeSize =
    shadowColumnIconSize // 3


showColumnButtonClass : String
showColumnButtonClass =
    "scshowbtn"


showColumnButtonWidth : Int
showColumnButtonWidth =
    70


showColumnButtonOcticonSize : Int
showColumnButtonOcticonSize =
    14


deleteColumnButtonClass : String
deleteColumnButtonClass =
    "scdelbtn"
