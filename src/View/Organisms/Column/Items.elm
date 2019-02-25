module View.Organisms.Column.Items exposing (render)

import Data.Column exposing (ColumnItem(..))
import Data.Item exposing (Item(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Attribute, Html, div)
import Html.Keyed
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
    , items : List ColumnItem
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
                    items
                        |> ListExtra.groupWhile shouldGroup
                        |> List.map (itemGroupKey props.timezone)
            in
            Html.Keyed.node "div" attrs <|
                (contents ++ [ loadMoreOrButtonTokenKey (eff.onLoadMoreClick props.columnId) props.hasMore ])


shouldGroup : ColumnItem -> ColumnItem -> Bool
shouldGroup newer older =
    case ( newer, older ) of
        ( Product _ (DiscordItem dNewer), Product _ (DiscordItem dOlder) ) ->
            shouldGroupDiscordMessage dNewer dOlder

        ( Product _ (SlackItem sNewer), Product _ (SlackItem sOlder) ) ->
            shouldGroupSlackMessage sNewer sOlder

        ( _, _ ) ->
            False


shouldGroupDiscordMessage : Discord.Message -> Discord.Message -> Bool
shouldGroupDiscordMessage dNewer dOlder =
    (dNewer.channelId == dOlder.channelId)
        && (dNewer.author == dOlder.author)
        && (ms dOlder.timestamp + groupingIntervalMillis > ms dNewer.timestamp)


groupingIntervalMillis : Int
groupingIntervalMillis =
    60000


shouldGroupSlackMessage : Slack.Message -> Slack.Message -> Bool
shouldGroupSlackMessage sNewer sOlder =
    (sNewer.conversation == sOlder.conversation)
        && (sNewer.author == sOlder.author)
        && (ms (Slack.getPosix sOlder) + groupingIntervalMillis > ms (Slack.getPosix sNewer))


itemGroupKey : Time.Zone -> List ColumnItem -> ( String, Html msg )
itemGroupKey tz groupedItems =
    ( "todo", none )


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
