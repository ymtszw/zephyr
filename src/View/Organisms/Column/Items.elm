module View.Organisms.Column.Items exposing (render)

import Data.Column exposing (ColumnItem(..))
import Data.Item exposing (Item(..))
import Data.Producer.Discord as Discord
import Data.Producer.Slack as Slack
import Html exposing (Attribute, Html)
import Html.Keyed
import ListExtra
import Time
import TimeExtra exposing (ms)
import View.Atoms.Layout exposing (..)
import View.Style exposing (none)


type alias Effects msg =
    { scrollAttrs : List (Attribute msg) -- From Scroll.scrollAttrs; can be empty
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
            none

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
                (contents ++ [ loadMoreKey props.columnId props.hasMore ])


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


loadMoreKey : String -> Bool -> ( String, Html msg )
loadMoreKey cId hasMore =
    -- This button is rarely visible due to auto adjusting.
    -- But sometimes appears around window resizing, rapid scrolling, or reaching bottom
    ( "todo", none )
