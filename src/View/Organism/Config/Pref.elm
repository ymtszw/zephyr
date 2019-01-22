module View.Organism.Config.Pref exposing (Effects, Props, ShadowColumn(..), ShadowColumnProps, render, styles)

import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Html, div, h3, img, p)
import Html.Attributes exposing (alt, class, src)
import Html.Keyed
import View.Atom.Border as Border
import View.Atom.Input as Input
import View.Atom.Layout exposing (..)
import View.Atom.Theme exposing (aubergine)
import View.Atom.Typography exposing (colorNote, serif, sizeTitle, t)
import View.ConfigPane.DiscordConfig as Discord
import View.Style exposing (..)


type alias Effects msg =
    { onZephyrModeChange : Bool -> msg
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
            shadowColumnsTable props.columnSlotsAvailable props.shadowColumns
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


shadowColumnsTable : Bool -> List ( ShadowColumnProps, ShadowColumn ) -> Html msg
shadowColumnsTable slotsAvailable shadowColumns =
    Html.Keyed.node "div" [ flexColumn, spacingColumn5 ] <|
        case shadowColumns of
            [] ->
                [ ( "shadowColumnEmpty", desc [ t "(Empty)" ] ) ]

            _ ->
                List.map (shadowColumnRowKey slotsAvailable) shadowColumns


shadowColumnRowKey : Bool -> ( ShadowColumnProps, ShadowColumn ) -> ( String, Html msg )
shadowColumnRowKey slotsAvailable ( scp, sc ) =
    Tuple.pair scp.id <|
        div
            [ flexRow
            , flexBasisAuto
            , flexCenter
            , padding2
            , spacingRow5
            , case sc of
                SlackSC _ ->
                    aubergine

                _ ->
                    noAttr
            ]
            [ shadowColumnIcon scp.description sc
            ]


shadowColumnIcon : String -> ShadowColumn -> Html msg
shadowColumnIcon description sc =
    case sc of
        FallbackSC ->
            abbrIcon description

        DiscordSC { mainChannelName, guildIcon } ->
            badgedIcon discordBadge <|
                case guildIcon of
                    Just src ->
                        imageIcon src "Discord guild icon"

                    Nothing ->
                        abbrIcon mainChannelName

        SlackSC { mainConvName, teamIcon } ->
            badgedIcon slackBadge <|
                case teamIcon of
                    Just src ->
                        imageIcon src "Slack team icon"

                    Nothing ->
                        abbrIcon mainConvName


badgedIcon : Html msg -> Html msg -> Html msg
badgedIcon badge icon =
    withBadge []
        { topRight = Nothing
        , bottomRight = Just badge
        , content = icon -- No inset
        }


abbrIcon : String -> Html msg
abbrIcon text =
    div
        [ class shadowColumnIconClass
        , flexColumn
        , flexCenter
        , flexBasisAuto
        , serif
        , Border.round2
        , Border.solid
        , Border.w1
        ]
        [ div [] [ t (String.left 1 text) ]
        ]


imageIcon : String -> String -> Html msg
imageIcon src_ alt_ =
    img
        [ class shadowColumnIconClass
        , Border.round2
        , src src_
        , alt alt_
        ]
        []


discordBadge : Html msg
discordBadge =
    imageBadge "Discord logo" <| Discord.defaultIconUrl (Just shadowColumnIconBadgeSize)


imageBadge : String -> String -> Html msg
imageBadge alt_ src_ =
    img [ class shadowColumnIconBadgeClass, src src_, alt alt_ ] []


slackBadge : Html msg
slackBadge =
    imageBadge "Slack logo" <| Slack.defaultIconUrl (Just shadowColumnIconBadgeSize)


styles : List Style
styles =
    [ s (c shadowColumnIconClass)
        [ ( "width", px shadowColumnIconSize )
        , ( "height", px shadowColumnIconSize )
        , ( "justify-content", "center" )
        ]
    , s (c shadowColumnIconBadgeClass)
        [ ( "width", px shadowColumnIconBadgeSize )
        , ( "height", px shadowColumnIconBadgeSize )
        ]
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
