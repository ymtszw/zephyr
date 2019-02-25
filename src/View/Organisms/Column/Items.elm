module View.Organisms.Column.Items exposing (render)

import Data.Column exposing (ColumnItem(..))
import Data.Item exposing (Item(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Attribute, Html, div)
import Html.Keyed
import List.Extra
import ListExtra
import Octicons
import Time
import TimeExtra exposing (ms)
import View.Atoms.Background as Background
import View.Atoms.Image as Image
import View.Atoms.Layout exposing (..)
import View.Atoms.Typography exposing (..)
import View.Molecules.Icon as Icon
import View.Style exposing (none)


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
    -- This button is rarely visible due to auto adjusting.
    -- But sometimes appears around window resizing, rapid scrolling, or reaching bottom
    Tuple.pair "loadMoreOrBottomToken" <|
        if hasMore then
            Icon.octiconButton [ flexItem, flexColumn, flexCenter, padding15, Background.transparent ]
                { onPress = onLoadMoreClick
                , size = xxProminentSize
                , shape = Octicons.commentDiscussion
                }

        else
            div [ flexColumn, flexCenter, padding15 ]
                [ Image.octicon
                    { size = xxProminentSize
                    , shape = Octicons.thumbsup
                    }
                ]



-- ITEM


itemGroupKey : Time.Zone -> ( ColumnItem, List ColumnItem ) -> ( String, Html msg )
itemGroupKey tz ( oldestItem, subsequentItems ) =
    ( "todo", none )
