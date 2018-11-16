module View.ColumnArea exposing (columnAreaEl)

import Array exposing (Array)
import ArrayExtra
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column exposing (ColumnItem(..))
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial)
import Data.Item exposing (Item(..))
import Data.Model exposing (Env, Model, ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Keyed
import Element.Lazy exposing (..)
import Html.Attributes exposing (draggable, id, style)
import Html.Events
import Json.Decode as D exposing (Decoder)
import ListExtra
import Octicons
import Scroll exposing (Scroll)
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
        , scrollbarX
        , Font.regular
        , htmlAttribute (id columnAreaParentId)
        , htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))
        ]
        (ColumnStore.mapForView (columnKeyEl m.env m.viewState) m.columnStore)


columnKeyEl : Env -> ViewState -> FilterAtomMaterial -> Int -> Column.Column -> ( String, Element Msg )
columnKeyEl env vs fam index c =
    let
        baseAttrs =
            [ width (px fixedColumnWidth)
            , height (fill |> maximum env.clientHeight)
            , BG.color oneDark.main
            , BD.width columnBorder
            , BD.color oneDark.bg
            , Font.color oneDark.text
            ]

        attrs =
            case vs.columnSwapMaybe of
                Just swap ->
                    if swap.grabbedId == c.id then
                        baseAttrs ++ [ inFront (lazy dragIndicatorEl env.clientHeight) ]

                    else
                        let
                            newOrder =
                                ArrayExtra.moveFromTo swap.originalIndex index swap.originalOrder
                        in
                        baseAttrs ++ [ htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter newOrder, True ))) ]

                Nothing ->
                    if vs.columnSwappable then
                        baseAttrs ++ dragHandle (D.succeed (DragStart index c.id))

                    else
                        baseAttrs
    in
    Tuple.pair c.id <|
        column attrs
            [ lazy2 columnHeaderEl fam c
            , lazy4 columnConfigFlyoutEl vs.selectState fam index c
            , lazy4 itemsEl env.clientHeight vs.timezone c.id c.items
            ]


columnBorder : Int
columnBorder =
    -- This border looks rather pointless, though we may introduce "focus" sytle later.
    2


columnHeaderEl : FilterAtomMaterial -> Column.Column -> Element Msg
columnHeaderEl fam c =
    row
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BG.color oneDark.sub
        ]
        [ filtersToIconEl [] { size = columnHeaderIconSize, fam = fam, filters = c.filters }
        , lazy4 columnHeaderTextEl fam c.id (Scroll.scrolled c.items) c.filters
        , lazy2 columnConfigToggleButtonEl c.configOpen c.id
        ]


columnHeaderIconSize : Int
columnHeaderIconSize =
    32


columnHeaderTextEl : FilterAtomMaterial -> String -> Bool -> Array Filter -> Element Msg
columnHeaderTextEl fam cId scrolled filters =
    let
        arrayReducer f acc =
            List.sortWith Filter.compareFilterAtom (Filter.toList f) :: acc
    in
    filters
        |> Array.foldr arrayReducer []
        |> List.concatMap (List.map (filterAtomTextEl fam))
        |> List.intersperse (breakT "  ")
        |> breakP
            [ centerY
            , Font.size baseHeaderTextSize
            , Font.color baseHeaderTextColor
            ]
        |> el
            ([ width fill, height fill ]
                ++ (if scrolled then
                        [ pointer, onClick (ColumnCtrl cId (Column.ScrollMsg Scroll.BackToTop)) ]

                    else
                        []
                   )
            )


baseHeaderTextSize : Int
baseHeaderTextSize =
    scale12 1


baseHeaderTextColor : Color
baseHeaderTextColor =
    oneDark.note


filterAtomTextEl : FilterAtomMaterial -> FilterAtom -> Element Msg
filterAtomTextEl fam fa =
    case fa of
        OfDiscordChannel cId ->
            FAM.mapDiscordChannel cId fam discordChannelTextEl
                |> Maybe.withDefault (breakT cId)

        ByMessage query ->
            breakT ("\"" ++ query ++ "\"")

        ByMedia HasImage ->
            octiconEl [] { size = importantFilterTextSize, color = baseHeaderTextColor, shape = Octicons.fileMedia }

        ByMedia HasMovie ->
            octiconEl [] { size = importantFilterTextSize, color = baseHeaderTextColor, shape = Octicons.deviceCameraVideo }

        ByMedia HasNone ->
            octiconEl [] { size = importantFilterTextSize, color = baseHeaderTextColor, shape = Octicons.textSize }

        RemoveMe ->
            none


discordChannelTextEl : Discord.ChannelCache -> Element Msg
discordChannelTextEl c =
    el [ Font.size importantFilterTextSize, Font.color importantFilterTextColor, Font.bold ] (breakT ("#" ++ c.name))


importantFilterTextSize : Int
importantFilterTextSize =
    scale12 2


importantFilterTextColor : Color
importantFilterTextColor =
    oneDark.text


columnConfigToggleButtonEl : Bool -> String -> Element Msg
columnConfigToggleButtonEl configOpen id =
    squareButtonEl [ alignRight, BD.rounded rectElementRound ]
        { onPress = ColumnCtrl id (Column.ToggleConfig (not configOpen))
        , enabled = True
        , innerElement =
            octiconEl []
                { size = columnConfigToggleButtonSize
                , color = defaultOcticonColor
                , shape = Octicons.settings
                }
        , innerElementSize = columnConfigToggleButtonSize
        }


columnConfigToggleButtonSize : Int
columnConfigToggleButtonSize =
    26


itemsEl : Int -> Time.Zone -> String -> Scroll ColumnItem -> Element Msg
itemsEl clientHeight tz cId items =
    if Scroll.isEmpty items then
        waitingForFirstItemEl

    else
        let
            columnAttrs =
                [ width fill, paddingXY rectElementInnerPadding 0, scrollbarY ]
                    ++ List.map htmlAttribute (Scroll.scrollAttrs (ColumnCtrl cId << Column.ScrollMsg) items)
        in
        -- Do note that items are sorted from latest to oldest
        items
            |> Scroll.toList
            |> ListExtra.groupWhile shouldGroup
            |> List.map (columnItemKeyEl tz)
            |> Element.Keyed.column columnAttrs


waitingForFirstItemEl : Element Msg
waitingForFirstItemEl =
    el [ width fill, height fill, paddingXY 5 0 ] <|
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
        && (ms dOlder.timestamp + groupingIntervalMillis > ms dNewer.timestamp)


groupingIntervalMillis : Int
groupingIntervalMillis =
    60000


dragIndicatorEl : Int -> Element Msg
dragIndicatorEl clientHeight =
    el
        [ width fill
        , height (px clientHeight)
        , BD.innerGlow oneDark.prim 10
        ]
        none
