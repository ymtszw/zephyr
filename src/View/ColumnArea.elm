module View.ColumnArea exposing (columnAreaEl)

import Data.ColorTheme exposing (oneDark)
import Data.Column as Column exposing (ColumnItem(..))
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Item exposing (Item(..))
import Data.Model exposing (Env, Model, ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events
import Element.Font as Font
import Element.Input
import Element.Keyed
import Element.Lazy exposing (..)
import Html.Attributes exposing (draggable, id, style)
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
        , htmlAttribute (id columnAreaParentId)
        , scrollbarX
        , Font.regular
        , htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))
        ]
        (ColumnStore.indexedMap (columnKeyEl m.env m.viewState) m.columnStore)


columnKeyEl : Env -> ViewState -> Int -> Column.Column -> ( String, Element Msg )
columnKeyEl env vs index c =
    let
        baseAttrs =
            [ width (px fixedColumnWidth)
            , height (fill |> maximum env.clientHeight)
            , BG.color oneDark.main
            , BD.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
            , BD.color oneDark.bg
            , Font.color oneDark.text
            ]

        attrs =
            case vs.columnSwapMaybe of
                Just swap ->
                    if swap.grabbedId == c.id then
                        baseAttrs ++ [ inFront (lazy dragIndicatorEl env.clientHeight) ]

                    else
                        baseAttrs ++ [ htmlAttribute (Html.Events.on "dragenter" (D.succeed (DragEnter index))) ]

                Nothing ->
                    if vs.columnSwappable then
                        baseAttrs ++ dragHandle (D.succeed (DragStart index c.id))

                    else
                        baseAttrs
    in
    Tuple.pair c.id <|
        column attrs
            [ lazy columnHeaderEl c
            , lazy4 columnConfigFlyoutEl vs.selectState vs.filterAtomMaterial index c
            , lazy2 itemsEl vs.timezone c.items
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


dragIndicatorEl : Int -> Element Msg
dragIndicatorEl clientHeight =
    el
        [ width fill
        , height (px clientHeight)
        , BD.innerGlow oneDark.prim 10
        ]
        none


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
