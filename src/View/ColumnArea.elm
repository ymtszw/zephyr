module View.ColumnArea exposing (columnAreaEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column exposing (ColumnItem(..))
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item(..))
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy2)
import Html.Attributes exposing (draggable, style)
import Html.Events
import Json.Decode as D exposing (Decoder)
import ListExtra
import Octicons
import Time
import TimeExtra exposing (ms)
import View.ColumnConfigFlyout exposing (columnConfigFlyoutEl)
import View.ColumnItem exposing (columnItemKeyEl)
import View.Parts exposing (..)


columnAreaEl : Model -> Element Msg
columnAreaEl m =
    Element.Keyed.row
        [ width fill
        , height (fill |> maximum m.env.clientHeight)
        , Font.regular
        ]
        (ColumnStore.indexedMap (columnKeyEl m) m.columnStore)


columnKeyEl : Model -> Int -> Column.Column -> ( String, Element Msg )
columnKeyEl m index column =
    Tuple.pair ("column_" ++ column.id) <|
        case m.viewState.columnSwapMaybe of
            Nothing ->
                notDraggedColumnEl m index column <|
                    if m.viewState.columnSwappable then
                        [ htmlAttribute (draggable "true")
                        , htmlAttribute (style "cursor" "all-scroll")
                        , htmlAttribute (Html.Events.on "dragstart" (onDragStart index column.id))
                        ]

                    else
                        []

            Just swap ->
                if swap.grabbedId == column.id then
                    draggedColumnEl m.env.clientHeight

                else
                    notDraggedColumnEl m index column <|
                        [ htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter index, True ))) ]


notDraggedColumnEl : Model -> Int -> Column.Column -> List (Attribute Msg) -> Element Msg
notDraggedColumnEl m index c attrs =
    column
        (columnBaseAttrs m.env.clientHeight ++ attrs)
        [ columnHeaderEl c
        , columnConfigFlyoutEl m index c
        , lazy2 itemsEl m.viewState.timezone c.items
        ]


itemsEl : Time.Zone -> List ColumnItem -> Element Msg
itemsEl tz items =
    case items of
        [] ->
            waitingForFirstItemEl

        _ ->
            -- Do note that items are sorted from latest to oldest
            items
                |> ListExtra.groupWhile shouldGroup
                |> List.map (columnItemKeyEl tz)
                |> Element.Keyed.column [ width fill, paddingXY 5 0, scrollbarY ]


waitingForFirstItemEl : Element Msg
waitingForFirstItemEl =
    el [ width fill, height (px 50), paddingXY 5 0 ] <|
        el [ centerX, centerY, Font.color oneDark.note, Font.size (scale12 2) ] <|
            text "Waiting for messages..."


shouldGroup : ColumnItem -> ColumnItem -> Bool
shouldGroup newer older =
    case ( newer, older ) of
        ( System _ _, _ ) ->
            False

        ( _, System _ _ ) ->
            False

        ( Product _ (DiscordItem dNewer), Product _ (DiscordItem dOlder) ) ->
            shouldGroupDiscordMessage dNewer dOlder


shouldGroupDiscordMessage : Discord.Message -> Discord.Message -> Bool
shouldGroupDiscordMessage dNewer dOlder =
    (dNewer.channelId == dOlder.channelId)
        && (dNewer.author == dOlder.author)
        && (ms dOlder.timestamp + 60000 > ms dNewer.timestamp)


draggedColumnEl : Int -> Element Msg
draggedColumnEl clientHeight =
    el (columnBaseAttrs clientHeight ++ [ BG.color oneDark.bg ]) none


columnBaseAttrs : Int -> List (Attribute Msg)
columnBaseAttrs clientHeight =
    [ width (px fixedColumnWidth)
    , height (fill |> maximum clientHeight)
    , BG.color oneDark.main
    , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
    , BD.color oneDark.bg
    , Font.color oneDark.text
    ]


onDragStart : Int -> String -> Decoder Msg
onDragStart index id =
    let
        fireDependingOnDataTransfer types =
            case types of
                [] ->
                    -- If Column div elements are dragged, it should not have items attached in dataTransfer property
                    D.succeed (DragStart index id)

                _ ->
                    -- Otherwise something else (img, link, etc...) are dragged. Turn off swap mode
                    D.succeed (ToggleColumnSwappable False)
    in
    D.at [ "dataTransfer", "types" ] (D.list D.string)
        |> D.andThen fireDependingOnDataTransfer


columnHeaderEl : Column.Column -> Element Msg
columnHeaderEl column =
    row
        [ width fill
        , padding 10
        , BG.color oneDark.sub
        ]
        [ text ("[PH] " ++ column.id)
        , Element.Input.button [ alignRight ]
            { onPress = Just (ToggleColumnConfig column.id (not column.configOpen))
            , label = octiconEl Octicons.settings
            }
        ]
