module Logger exposing (Entry, History, Msg(..), MsgFilter(..), historyEl, init, push, update)

import BoundedDeque exposing (BoundedDeque)
import Browser.Dom
import Data.ColorTheme exposing (oneDark)
import Data.UniqueId as UniqueId
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Element.Input
import Element.Keyed
import Element.Lazy exposing (lazy)
import Extra exposing (doAfter, ite)
import Html.Attributes exposing (id, readonly, style, tabindex)
import Html.Events
import Json.Decode as D
import Octicons
import StringExtra
import Task
import Time exposing (Posix)
import View.Parts exposing (..)



-- Types


type History
    = History
        { buffer : BoundedDeque ( String, Entry )
        , pending : List ( String, Entry )
        , scroll : Scroll
        , payloadFilter : String
        , msgFilters : List MsgFilter
        }


type alias Entry =
    { ctor : String -- Must not include whitespaces
    , payload : List String
    }


type Scroll
    = Scrolling Browser.Dom.Viewport
    | OffTheTop Browser.Dom.Viewport
    | AtTop


type Msg
    = ScrollStart
    | ViewportResult (Result Browser.Dom.Error ( Posix, Browser.Dom.Viewport ))
    | BackToTop
    | FilterInput String
    | SetMsgFilter MsgFilter
    | DelMsgFilter MsgFilter
    | NoOp


type MsgFilter
    = MsgFilter Bool String


init : History
init =
    History
        { buffer = BoundedDeque.empty historyLimit
        , pending = []
        , scroll = AtTop
        , payloadFilter = ""
        , msgFilters = defaultFilters
        }


historyLimit : Int
historyLimit =
    1000


defaultFilters : List MsgFilter
defaultFilters =
    -- Timer ticks are good candidates of default filters
    [ MsgFilter False "Tick"
    , MsgFilter False "Logger.ScrollStart"
    , MsgFilter False "Logger.ViewportOk"
    ]



-- Component


update : Msg -> History -> ( History, Cmd Msg )
update msg (History h) =
    case ( msg, h.scroll ) of
        ( ScrollStart, OffTheTop vp ) ->
            ( History { h | scroll = Scrolling vp }, queryViewport )

        ( ScrollStart, _ ) ->
            ( History h, queryViewport )

        ( BackToTop, _ ) ->
            ( History h, Browser.Dom.setViewportOf historyElementId 0 0 |> Task.attempt (\_ -> ScrollStart) )

        ( ViewportResult (Ok ( _, newVp )), Scrolling oldVp ) ->
            if newVp.viewport.y == 0 then
                ( History { h | scroll = AtTop } |> pendingToBuffer, Cmd.none )

            else if newVp == oldVp then
                ( History { h | scroll = OffTheTop newVp }, Cmd.none )

            else
                ( History { h | scroll = Scrolling newVp }, queryViewport )

        ( ViewportResult (Ok ( _, newVp )), _ ) ->
            if newVp.viewport.y == 0 then
                ( History { h | scroll = AtTop } |> pendingToBuffer, Cmd.none )

            else
                ( History { h | scroll = OffTheTop newVp }, Cmd.none )

        ( ViewportResult (Err _), _ ) ->
            ( History { h | scroll = AtTop }, Cmd.none )

        ( FilterInput pf, _ ) ->
            ( History { h | payloadFilter = pf }, Cmd.none )

        ( SetMsgFilter mf, _ ) ->
            ( setMsgFilter mf (History h), Cmd.none )

        ( DelMsgFilter mf, _ ) ->
            ( History { h | msgFilters = List.filter ((/=) mf) h.msgFilters }, Cmd.none )

        ( NoOp, _ ) ->
            ( History h, Cmd.none )


queryViewport : Cmd Msg
queryViewport =
    doAfter 50 ViewportResult (Browser.Dom.getViewportOf historyElementId)


pendingToBuffer : History -> History
pendingToBuffer (History h) =
    History <|
        { h
            | buffer = List.foldr BoundedDeque.pushFront h.buffer h.pending
            , pending = []
        }


setMsgFilter : MsgFilter -> History -> History
setMsgFilter ((MsgFilter isPos msg) as mf) (History h) =
    History <|
        if List.member mf h.msgFilters then
            h

        else if List.member (MsgFilter (not isPos) msg) h.msgFilters then
            { h
                | msgFilters =
                    h.msgFilters
                        |> List.filter ((/=) (MsgFilter (not isPos) msg))
                        |> (::) mf
            }

        else
            { h | msgFilters = mf :: h.msgFilters }



-- APIs


push : UniqueId.Generator -> Entry -> History -> ( History, UniqueId.Generator )
push idGen e (History h) =
    UniqueId.genAndMap "logEntry" idGen <|
        \eId ->
            case h.scroll of
                AtTop ->
                    pendingToBuffer (History h) |> pushToBuffer ( eId, e )

                _ ->
                    pushToPending ( eId, e ) (History h)


pushToBuffer : ( String, Entry ) -> History -> History
pushToBuffer ( eId, e ) (History h) =
    History <|
        case BoundedDeque.popFront h.buffer of
            ( Just ( _, top ), popped ) ->
                if top.ctor == e.ctor then
                    { h | buffer = BoundedDeque.pushFront ( eId, e ) popped }

                else
                    { h | buffer = BoundedDeque.pushFront ( eId, e ) h.buffer }

            ( Nothing, _ ) ->
                { h | buffer = BoundedDeque.pushFront ( eId, e ) h.buffer }


pushToPending : ( String, Entry ) -> History -> History
pushToPending ( eId, e ) (History h) =
    History <|
        case h.pending of
            [] ->
                { h | pending = [ ( eId, e ) ] }

            ( _, p ) :: ps ->
                if p.ctor == e.ctor then
                    { h | pending = ( eId, e ) :: ps }

                else
                    { h | pending = ( eId, e ) :: h.pending }



-- View


historyEl : History -> Element Msg
historyEl h =
    column
        [ width fill
        , padding rectElementOuterPadding
        , spacing spacingUnit
        , BD.rounded rectElementRound
        , BG.color historyBackground
        , Font.size historyFontSize
        , inFront (newEntryToastEl h)
        ]
        [ historyTableEl h
        , msgFiltersEl h
        , payloadFilterInputEl h
        ]


historyBackground : Color
historyBackground =
    oneDark.sub


historyFontSize : Int
historyFontSize =
    scale12 1


historyTableEl : History -> Element Msg
historyTableEl (History h) =
    let
        ( negMsgFilters, posMsgFilters ) =
            List.partition (\(MsgFilter isPos _) -> not isPos) h.msgFilters

        payloadQueries =
            h.payloadFilter |> String.split " " |> List.filter (not << String.isEmpty)

        data =
            BoundedDeque.toList h.buffer
                |> List.filter (filterEntry negMsgFilters posMsgFilters payloadQueries)

        columnAttrs =
            [ width fill
            , height (shrink |> maximum historyTableMaxHeight)
            , padding rectElementInnerPadding
            , spacing historyTableCellSpacing
            , clipX
            , BG.color historyTableBackground
            , htmlAttribute (id historyElementId)
            , detectScroll (History h)
            ]
    in
    Element.Keyed.column columnAttrs <|
        ( "LogHeaders"
        , row [ width fill, spacing historyTableCellSpacing ]
            [ el [ width ctorColumnWidth, BG.color historyTableHeaderBackground ] <| text "Msg"
            , el [ width payloadColumnWidth, BG.color historyTableHeaderBackground ] <| text "Payload"
            ]
        )
            :: List.map (rowKeyEl h.msgFilters) data


historyTableBackground : Color
historyTableBackground =
    oneDark.main


historyTableMaxHeight : Int
historyTableMaxHeight =
    400


historyTableCellSpacing : Int
historyTableCellSpacing =
    2


historyTableHeaderBackground : Color
historyTableHeaderBackground =
    oneDark.note


historyElementId : String
historyElementId =
    "loggerHistory"


ctorColumnWidth : Length
ctorColumnWidth =
    fillPortion 1


payloadColumnWidth : Length
payloadColumnWidth =
    fillPortion 2


detectScroll : History -> Element.Attribute Msg
detectScroll (History h) =
    case h.scroll of
        AtTop ->
            htmlAttribute <| Html.Events.on "scroll" (D.succeed ScrollStart)

        OffTheTop _ ->
            htmlAttribute <| Html.Events.on "scroll" (D.succeed ScrollStart)

        Scrolling _ ->
            noneAttr


filterEntry : List MsgFilter -> List MsgFilter -> List String -> ( String, Entry ) -> Bool
filterEntry negMsgFilters posMsgFilters payloadQueries ( _, e ) =
    if negMsgFilters == [] || not (List.member (MsgFilter False e.ctor) negMsgFilters) then
        if posMsgFilters == [] || List.member (MsgFilter True e.ctor) posMsgFilters then
            payloadQueries == [] || List.any (\q -> List.any (String.contains q) e.payload) payloadQueries

        else
            False

    else
        False


newEntryToastEl : History -> Element Msg
newEntryToastEl (History h) =
    case h.pending of
        [] ->
            none

        ps ->
            el [ width fill, alignTop, padding rectElementOuterPadding ] <|
                Element.Input.button [ width fill, padding rectElementInnerPadding, BG.color oneDark.succ ]
                    { onPress = Just BackToTop
                    , label = el [ centerX ] <| text ("New Log Entry (" ++ String.fromInt (List.length ps) ++ ")")
                    }


rowKeyEl : List MsgFilter -> ( String, Entry ) -> ( String, Element Msg )
rowKeyEl msgFilters ( eId, e ) =
    ( eId
    , row [ width fill, spacing historyTableCellSpacing ]
        [ el [ alignTop, width ctorColumnWidth ] <| ctorCellEl msgFilters e
        , el [ alignTop, width payloadColumnWidth ] <| payloadCellsEl e
        ]
    )


ctorCellEl : List MsgFilter -> Entry -> Element Msg
ctorCellEl msgFilters entry =
    row [ spacing spacingUnit ]
        [ breakP [ bold ] [ breakT entry.ctor ]
        , Element.Input.button [ htmlAttribute (tabindex -1) ] <|
            if List.member (MsgFilter True entry.ctor) msgFilters then
                { onPress = Just (DelMsgFilter (MsgFilter True entry.ctor))
                , label = el [ BG.color oneDark.succ ] <| octiconFreeSizeEl (scale12 1) Octicons.diffAdded
                }

            else
                { onPress = Just (SetMsgFilter (MsgFilter True entry.ctor))
                , label = el [] <| octiconFreeSizeEl (scale12 1) Octicons.diffAdded
                }
        , Element.Input.button [ htmlAttribute (tabindex -1) ] <|
            -- No need for switch since if Negative Filter is set, this entry should be invisible
            { onPress = Just (SetMsgFilter (MsgFilter False entry.ctor))
            , label = el [] <| octiconFreeSizeEl (scale12 1) Octicons.diffRemoved
            }
        ]


payloadCellsEl : Entry -> Element Msg
payloadCellsEl entry =
    column [ width fill, spacing historyTableCellSpacing ] (List.map payloadCellEl entry.payload)


payloadCellEl : String -> Element Msg
payloadCellEl raw =
    Element.Input.multiline
        [ width fill
        , height (shrink |> maximum payloadCellMaxHeight)
        , padding rectElementInnerPadding
        , focused []
        , BD.width 0
        , BG.color payloadCellBackground
        , Font.family [ Font.typeface "consolas", Font.monospace ]
        , htmlAttribute (style "line-height" "1") -- Cancelling line-height introduced by elm-ui
        , htmlAttribute (readonly True)
        , htmlAttribute (tabindex -1)
        ]
        { onChange = always NoOp
        , text = raw
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Payload"
        , spellcheck = False
        }


payloadCellBackground : Color
payloadCellBackground =
    oneDark.bg


payloadCellMaxHeight : Int
payloadCellMaxHeight =
    100


msgFiltersEl : History -> Element Msg
msgFiltersEl (History h) =
    wrappedRow [ width fill, spacing spacingUnit ] <|
        List.map msgFilterEl h.msgFilters


msgFilterEl : MsgFilter -> Element Msg
msgFilterEl ((MsgFilter isPos ctor) as mf) =
    row
        [ width shrink
        , BD.rounded rectElementRound
        , BG.color (ite isPos oneDark.succ oneDark.err)
        ]
        [ el
            [ padding msgFilterPadding
            , BD.roundEach { topLeft = rectElementRound, bottomLeft = rectElementRound, topRight = 0, bottomRight = 0 }
            ]
            (text ctor)
        , Element.Input.button
            [ padding msgFilterPadding
            , focused []
            , htmlAttribute (tabindex -1)
            , BD.roundEach { topLeft = 0, bottomLeft = 0, topRight = rectElementRound, bottomRight = rectElementRound }
            ]
            { onPress = Just (DelMsgFilter mf)
            , label = octiconFreeSizeEl msgFilterDeleteIconSize Octicons.trashcan
            }
        ]


msgFilterPadding : Int
msgFilterPadding =
    2


msgFilterDeleteIconSize : Int
msgFilterDeleteIconSize =
    scale12 2


payloadFilterInputEl : History -> Element Msg
payloadFilterInputEl (History h) =
    textInputEl
        { onChange = FilterInput
        , theme = oneDark
        , enabled = True
        , text = h.payloadFilter
        , label = Element.Input.labelHidden "Log Payload Filter"
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    el [ centerY ] <|
                        text "Payload OR Filter (Space-delimited, Case-sensitive)"
        }
