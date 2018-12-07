module View.ColumnArea exposing (columnAreaEl)

import Array exposing (Array)
import ArrayExtra
import Data.ColorTheme exposing (oneDark)
import Data.Column as Column exposing (ColumnItem(..))
import Data.ColumnStore as ColumnStore exposing (ColumnStore)
import Data.Filter as Filter exposing (Filter, FilterAtom(..), MediaFilter(..))
import Data.FilterAtomMaterial as FAM exposing (FilterAtomMaterial)
import Data.Item exposing (Item(..))
import Data.Model exposing (ColumnSwap, Env, Model, ViewState)
import Data.Msg exposing (Msg(..))
import Data.Producer.Discord as Discord
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Keyed
import Element.Lazy exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder)
import ListExtra
import Octicons
import Scroll exposing (Scroll)
import Time
import TimeExtra exposing (ms)
import View.ColumnConfigFlyout exposing (columnConfigFlyoutEl)
import View.ColumnItem exposing (columnItemKeyEl)
import View.NewMessageEditor exposing (newMessageEditorEl)
import View.Parts exposing (..)


columnAreaEl : Model -> Element Msg
columnAreaEl m =
    Element.Keyed.row
        [ width fill
        , height (fill |> maximum m.env.clientHeight)
        , scrollbarX
        , Font.regular
        , htmlAttribute (Html.Attributes.id columnAreaParentId)
        , htmlAttribute (Html.Events.on "dragend" (D.succeed DragEnd))
        ]
        (ColumnStore.mapForView (columnKeyEl m) m.columnStore)


columnKeyEl : Model -> FilterAtomMaterial -> Int -> Column.Column -> ( String, Element Msg )
columnKeyEl m fam index c =
    let
        baseAttrs =
            [ width (px columnWidth)
            , height fill
            , clipY
            , BG.color oneDark.main
            , BD.width columnBorderWidth
            , BD.color oneDark.bg
            , Font.color oneDark.text
            , borderFlash c.recentlyTouched
            , onAnimationEnd (ColumnCtrl c.id Column.Calm)
            , style "transition" "all 0.15s"
            ]
    in
    Tuple.pair c.id <|
        column (baseAttrs ++ dragAttributes m.env.clientHeight m.viewState.columnSwapMaybe index c)
            [ lazy3 columnHeaderEl fam index c
            , lazy4 columnConfigFlyoutEl m.viewState.selectState fam index c
            , newMessageEditorEl m.viewState.selectState fam c
            , lazy3 itemsEl m.viewState.timezone c.id c.items
            , fillerEl
            ]


dragAttributes : Int -> Maybe ColumnSwap -> Int -> Column.Column -> List (Attribute Msg)
dragAttributes clientHeight columnSwapMaybe index c =
    -- Here we change styles of big and complex DOMs; must consider performance carefully.
    -- CSS opacity/transform utilizes GPU support so are quite fast and cheap
    case columnSwapMaybe of
        Just swap ->
            if swap.grabbedId == c.id then
                [ inFront (lazy2 dragIndicatorEl clientHeight True)
                , htmlAttribute (Html.Events.preventDefaultOn "dragover" (D.succeed ( NoOp, True )))
                ]

            else if swap.pinned == c.pinned then
                let
                    newOrder =
                        ArrayExtra.moveFromTo swap.originalIndex index swap.originalOrder
                in
                [ inFront (lazy2 dragIndicatorEl clientHeight False)
                , htmlAttribute (Html.Events.preventDefaultOn "dragenter" (D.succeed ( DragEnter newOrder, True )))
                , htmlAttribute (Html.Events.preventDefaultOn "dragover" (D.succeed ( NoOp, True )))
                , style "transform" "scale(0.98)"
                ]

            else
                [ inFront (lazy2 dragIndicatorEl clientHeight False)
                , style "opacity" "0.2"
                ]

        Nothing ->
            [ inFront (lazy2 dragIndicatorEl clientHeight False) ]


dragIndicatorEl : Int -> Bool -> Element Msg
dragIndicatorEl clientHeight grabbed =
    el
        [ width fill
        , height (px clientHeight)
        , BD.innerGlow oneDark.prim 10
        , visible grabbed
        ]
        none


fillerEl : Element Msg
fillerEl =
    el [ width fill, height (fill |> minimum 0) ] none


columnHeaderEl : FilterAtomMaterial -> Int -> Column.Column -> Element Msg
columnHeaderEl fam index c =
    row
        [ width fill
        , padding rectElementInnerPadding
        , spacing spacingUnit
        , BG.color columnHeaderBackground
        ]
        [ lazy3 grabberEl index c.pinned c.id
        , filtersToIconEl [] { size = columnHeaderIconSize, fam = fam, filters = c.filters }
        , lazy4 columnHeaderTextEl fam c.id (Scroll.scrolled c.items) c.filters
        , lazy2 columnDismissButtonEl c.pinned index
        , lazy2 columnPinButtonEl c.pinned c.id
        , lazy2 columnConfigToggleButtonEl c.configOpen c.id
        ]


columnHeaderBackground : Color
columnHeaderBackground =
    oneDark.sub


grabberEl : Int -> Bool -> String -> Element Msg
grabberEl index pinned cId =
    let
        attrs =
            [ width (px grabberWidth)
            , height fill
            , BG.color oneDark.main
            , BD.rounded (grabberWidth // 2)
            , BD.width (grabberWidth // 2)
            , BD.color oneDark.note
            , style "border-style" "double"
            ]

        grabberAttrs =
            dragHandle <| D.succeed <| DragStart { index = index, pinned = pinned, id = cId }
    in
    el (attrs ++ grabberAttrs) none


grabberWidth : Int
grabberWidth =
    8


columnHeaderTextEl : FilterAtomMaterial -> String -> Bool -> Array Filter -> Element Msg
columnHeaderTextEl fam cId scrolled filters =
    let
        backToTopAttrs =
            if scrolled then
                [ pointer, onClick (ColumnCtrl cId (Column.ScrollMsg Scroll.BackToTop)) ]

            else
                []
    in
    el ([ width fill, height fill ] ++ backToTopAttrs) <|
        filtersToTextEl
            [ centerY
            , Font.size baseHeaderTextSize
            , Font.color baseHeaderTextColor
            ]
            { fontSize = importantFilterTextSize
            , color = importantFilterTextColor
            , fam = fam
            , filters = filters
            }


baseHeaderTextSize : Int
baseHeaderTextSize =
    scale12 1


baseHeaderTextColor : Color
baseHeaderTextColor =
    oneDark.note


importantFilterTextSize : Int
importantFilterTextSize =
    scale12 2


importantFilterTextColor : Color
importantFilterTextColor =
    oneDark.text


columnDismissButtonEl : Bool -> Int -> Element Msg
columnDismissButtonEl pinned index =
    squareButtonEl [ alignRight, BD.rounded rectElementRound, visible (not pinned) ]
        { onPress = DismissColumn index
        , enabled = True
        , innerElement =
            octiconEl [ mouseOver [ BG.color oneDark.succ ] ]
                { size = rightButtonSize
                , color = defaultOcticonColor
                , shape = Octicons.check
                }
        , innerElementSize = rightButtonSize
        }


columnPinButtonEl : Bool -> String -> Element Msg
columnPinButtonEl pinned cId =
    squareButtonEl [ alignRight, BD.rounded rectElementRound ]
        { onPress = ColumnCtrl cId (Column.Pin (not pinned))
        , enabled = True
        , innerElement =
            octiconEl
                [ style "transition" "transform 0.2s"
                , style "transform" <|
                    if pinned then
                        "rotate(-45deg)"

                    else
                        "rotate(0)"
                ]
                { size = rightButtonSize
                , color =
                    if pinned then
                        columnPinColor

                    else
                        defaultOcticonColor
                , shape = Octicons.pin
                }
        , innerElementSize = rightButtonSize
        }


rightButtonSize : Int
rightButtonSize =
    26


columnConfigToggleButtonEl : Bool -> String -> Element Msg
columnConfigToggleButtonEl configOpen id =
    squareButtonEl [ alignRight, BD.rounded rectElementRound ]
        { onPress = ColumnCtrl id (Column.ToggleConfig (not configOpen))
        , enabled = True
        , innerElement =
            octiconEl []
                { size = rightButtonSize
                , color =
                    if configOpen then
                        oneDark.text

                    else
                        defaultOcticonColor
                , shape = Octicons.settings
                }
        , innerElementSize = rightButtonSize
        }


itemsEl : Time.Zone -> String -> Scroll ColumnItem -> Element Msg
itemsEl tz cId items =
    if Scroll.isEmpty items then
        waitingForFirstItemEl

    else
        let
            columnAttrs =
                [ width fill
                , height shrink
                , paddingXY rectElementInnerPadding 0
                , alignTop
                , scrollbarY
                ]
                    ++ List.map htmlAttribute (Scroll.scrollAttrs (ColumnCtrl cId << Column.ScrollMsg) items)

            itemsVisible =
                Scroll.toList items

            hasMore =
                List.length itemsVisible < Scroll.size items
        in
        -- Do note that items are sorted from latest to oldest
        itemsVisible
            |> ListExtra.groupWhile shouldGroup
            |> List.map (columnItemKeyEl tz)
            |> (\itemEls -> itemEls ++ [ loadMoreKeyEl cId hasMore ])
            |> Element.Keyed.column columnAttrs


waitingForFirstItemEl : Element Msg
waitingForFirstItemEl =
    el [ width fill, padding rectElementOuterPadding, alignTop ] <|
        el [ centerX, centerY, Font.color oneDark.note, Font.size helpTextSize ] <|
            text "Waiting for messages..."


helpTextSize : Int
helpTextSize =
    scale12 2


shouldGroup : ColumnItem -> ColumnItem -> Bool
shouldGroup newer older =
    case ( newer, older ) of
        ( Product _ (DiscordItem dNewer), Product _ (DiscordItem dOlder) ) ->
            shouldGroupDiscordMessage dNewer dOlder

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


loadMoreKeyEl : String -> Bool -> ( String, Element Msg )
loadMoreKeyEl cId hasMore =
    -- This button is rarely visible due to auto adjusting.
    -- But sometimes appears around window resizing, rapid scrolling, or reaching bottom
    Tuple.pair "loadMoreOrBottomToken" <|
        el [ width fill, height (shrink |> minimum columnItemMinimumHeight), padding rectElementOuterPadding ] <|
            if hasMore then
                octiconEl
                    [ centerX
                    , centerY
                    , pointer
                    , onClick (ColumnCtrl cId (Column.ScrollMsg Scroll.LoadMore))
                    ]
                    { size = helpTextSize + rectElementOuterPadding * 2
                    , color = oneDark.note
                    , shape = Octicons.commentDiscussion
                    }

            else
                octiconEl [ centerX, centerY ]
                    { size = helpTextSize + rectElementOuterPadding * 2
                    , color = oneDark.note
                    , shape = Octicons.thumbsup
                    }
