module View.Organisms.Column.Items exposing (render, styles)

import Broker
import Data.Column exposing (ColumnItem(..), Media)
import Data.Item exposing (Item(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Html.Keyed
import List.Extra
import Octicons
import Time
import TimeExtra exposing (ms)
import Url
import View.Atoms.Border as Border
import View.Atoms.Cursor as Cursor
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (..)


type alias Effects msg =
    { scrollAttrs : List (Attribute msg) -- From Scroll.scrollAttrs; can be empty
    , onLoadMoreClick : String -> msg
    }


type alias Props =
    { timezone : Time.Zone
    , columnId : String
    , items : List ColumnItem -- Expects it to be sorted from latest to oldest
    , hasMore : Bool
    }


render : Effects msg -> Props -> Html msg
render eff props =
    case props.items of
        [] ->
            div [ flexColumn, flexCenter, padding15, colorNote ] [ t "Waiting for messages..." ]

        items ->
            let
                attrs =
                    [ flexBasisAuto
                    , flexShrink
                    , flexColumn
                    , padding5
                    ]
                        ++ eff.scrollAttrs

                contents =
                    -- We reverse first, since we want to group items in "older to newer" order, while gloabally showing "newest to oldest"
                    List.reverse items
                        |> List.Extra.groupWhile shouldGroup
                        |> List.Extra.reverseMap (itemGroupKey props.timezone)
            in
            Html.Keyed.node "div" attrs <|
                (contents ++ [ loadMoreOrButtonTokenKey (eff.onLoadMoreClick props.columnId) props.hasMore ])


shouldGroup : ColumnItem -> ColumnItem -> Bool
shouldGroup older newer =
    case ( older, newer ) of
        ( Product _ (DiscordItem dOlder), Product _ (DiscordItem dNewer) ) ->
            shouldGroupDiscordMessage dOlder dNewer

        ( Product _ (SlackItem sOlder), Product _ (SlackItem sNewer) ) ->
            shouldGroupSlackMessage sOlder sNewer

        ( _, _ ) ->
            False


shouldGroupDiscordMessage : Discord.Message -> Discord.Message -> Bool
shouldGroupDiscordMessage dOlder dNewer =
    (dNewer.channelId == dOlder.channelId)
        && (dNewer.author == dOlder.author)
        && (ms dOlder.timestamp + groupingIntervalMillis > ms dNewer.timestamp)


groupingIntervalMillis : Int
groupingIntervalMillis =
    60000


shouldGroupSlackMessage : Slack.Message -> Slack.Message -> Bool
shouldGroupSlackMessage sOlder sNewer =
    (sNewer.conversation == sOlder.conversation)
        && (sNewer.author == sOlder.author)
        && (ms (Slack.getPosix sOlder) + groupingIntervalMillis > ms (Slack.getPosix sNewer))


loadMoreOrButtonTokenKey : msg -> Bool -> ( String, Html msg )
loadMoreOrButtonTokenKey onLoadMoreClick hasMore =
    -- This clickable area is rarely visible due to auto adjusting.
    -- But sometimes appears around window resizing, rapid scrolling, or reaching bottom
    let
        ( shape, additionalAttrs ) =
            if hasMore then
                ( Octicons.commentDiscussion
                , [ title "Scroll, or click here for more...", onClick onLoadMoreClick, Cursor.pointer ]
                )

            else
                ( Octicons.thumbsup, [ title "All read!" ] )
    in
    Tuple.pair "loadMoreOrBottomToken" <|
        div ([ flexColumn, flexCenter, padding15 ] ++ additionalAttrs)
            [ Image.octicon
                { size = xxProminentSize
                , shape = shape
                }
            ]



-- ITEM


itemGroupKey : Time.Zone -> ( ColumnItem, List ColumnItem ) -> ( String, Html msg )
itemGroupKey tz ( oldestItem, subsequentItems ) =
    let
        key =
            (++) "itemGroup_" <|
                case oldestItem of
                    Product offset _ ->
                        Broker.offsetToString offset

                    SystemMessage { id } ->
                        id

                    LocalMessage { id } ->
                        id
    in
    Tuple.pair key <|
        div
            [ class itemGroupClass
            , flexRow
            , flexBasisAuto
            , spacingRow5
            , Border.bot1
            , Border.solid
            , Border.colorBd
            ]
            [ itemAuthorAvatar40 oldestItem
            , itemGroupContents tz oldestItem subsequentItems
            ]


itemAuthorAvatar40 : ColumnItem -> Html msg
itemAuthorAvatar40 item =
    let
        badgedAvatar40 badgeMaybe description srcMaybe =
            withBadge [ badgeOutset ]
                { topRight = Nothing
                , bottomRight = badgeMaybe
                , content = Icon.imgOrAbbr [ serif, xProminent, Icon.rounded40 ] description srcMaybe
                }

        octiconAvatar40 shape =
            Icon.octiconBlock
                [ Icon.rounded40
                , Border.colorNote
                ]
                { size = xProminentSize
                , shape = shape
                }
    in
    case item of
        Product _ (DiscordItem { author }) ->
            let
                ( user, badgeMaybe ) =
                    case author of
                        Discord.UserAuthor u ->
                            ( u, Nothing )

                        Discord.WebhookAuthor u ->
                            ( u, Just Icon.botBadge14 )
            in
            badgedAvatar40 badgeMaybe user.username <|
                Just (Icon.discordImageUrlWithFallback40 user.discriminator user.avatar)

        Product _ (SlackItem m) ->
            let
                ( name, srcMaybe, badgeMaybe ) =
                    case m.author of
                        Slack.UserAuthor u ->
                            ( Maybe.withDefault u.profile.realName u.profile.displayName
                            , Just (Url.toString u.profile.image48)
                            , Nothing
                            )

                        Slack.UserAuthorId (Slack.UserId str) ->
                            ( str, Nothing, Nothing )

                        Slack.BotAuthor b ->
                            ( Maybe.withDefault b.name m.username
                            , Just (Url.toString b.icons.image48)
                            , Just Icon.botBadge14
                            )

                        Slack.BotAuthorId (Slack.BotId str) ->
                            ( str, Nothing, Just Icon.botBadge14 )
            in
            badgedAvatar40 badgeMaybe name srcMaybe

        SystemMessage _ ->
            octiconAvatar40 Octicons.info

        LocalMessage _ ->
            octiconAvatar40 Octicons.note


itemGroupContents : Time.Zone -> ColumnItem -> List ColumnItem -> Html msg
itemGroupContents tz oldestItem subsequentItems =
    Html.Keyed.node "div" [ flexColumn, flexBasisAuto, flexShrink, spacingColumn2 ] <|
        case oldestItem of
            SystemMessage { id, message, mediaMaybe } ->
                [ simpleMessageKey id message mediaMaybe ]

            LocalMessage { id, message } ->
                [ simpleMessageKey id message Nothing ]

            Product offset (DiscordItem oldestMessage) ->
                let
                    subsequentContents =
                        List.filterMap unwrap subsequentItems

                    unwrap item =
                        case item of
                            Product o (DiscordItem m) ->
                                Just (discordMessageKey o m)

                            _ ->
                                Nothing
                in
                [ ( "itemGroupHeader", discordItemGroupHeader tz oldestMessage )
                , discordMessageKey offset oldestMessage
                ]
                    ++ subsequentContents

            Product offset (SlackItem oldestMessage) ->
                let
                    subsequentContents =
                        List.filterMap unwrap subsequentItems

                    unwrap item =
                        case item of
                            Product o (SlackItem m) ->
                                Just (slackMessageKey o m)

                            _ ->
                                Nothing
                in
                [ ( "itemGroupHeader", slackItemGroupHeader tz oldestMessage )
                , slackMessageKey offset oldestMessage
                ]
                    ++ subsequentContents


discordItemGroupHeader : Time.Zone -> Discord.Message -> Html msg
discordItemGroupHeader tz dm =
    -- TODO
    none


slackItemGroupHeader : Time.Zone -> Slack.Message -> Html msg
slackItemGroupHeader tz sm =
    -- TODO
    none


simpleMessageKey : String -> String -> Maybe Media -> ( String, Html msg )
simpleMessageKey id message mediaMaybe =
    -- TODO
    Tuple.pair id none


discordMessageKey : Broker.Offset -> Discord.Message -> ( String, Html msg )
discordMessageKey os dm =
    -- TODO
    Tuple.pair (Broker.offsetToString os) none


slackMessageKey : Broker.Offset -> Slack.Message -> ( String, Html msg )
slackMessageKey os sm =
    -- TODO
    Tuple.pair (Broker.offsetToString os) none



-- STYLES


styles : List Style
styles =
    [ s (c itemGroupClass)
        [ ( "padding-top", px itemGroupPaddingY )
        , ( "padding-bottom", px itemGroupPaddingY )
        ]
    ]


itemGroupClass : String
itemGroupClass =
    "cig"


itemGroupPaddingY : Int
itemGroupPaddingY =
    5
