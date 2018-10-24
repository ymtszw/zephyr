module Logger exposing (Entry, History, Msg(..), MsgFilter(..), historyEl, init, rec, update)

import BoundedDeque exposing (BoundedDeque)
import Browser.Dom exposing (Viewport, getViewportOf, setViewportOf)
import Data.ColorTheme exposing (oneDark)
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font exposing (bold)
import Element.Input
import Extra exposing (doAfter, ite)
import Html.Attributes exposing (id, readonly, style, tabindex)
import Html.Events
import Json.Decode as D
import Octicons
import StringExtra
import Task
import Time exposing (Posix)
import View.Parts exposing (noneAttr, octiconFreeSizeEl, scale12)



-- Types


type History
    = History
        { buffer : BoundedDeque Entry
        , pending : List Entry
        , scroll : Scroll
        , payloadFilter : String
        , msgFilters : List MsgFilter
        }


type alias Entry =
    { ctor : String -- Must not include whitespaces
    , payload : List String
    }


type Scroll
    = Scrolling Viewport
    | OffTheTop Viewport
    | AtTop


type Msg
    = ScrollStart
    | ViewportResult (Result Browser.Dom.Error ( Posix, Viewport ))
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
        , msgFilters = []
        }


historyLimit : Int
historyLimit =
    1000



-- Component


update : Msg -> History -> ( History, Cmd Msg )
update msg (History h) =
    case ( msg, h.scroll ) of
        ( ScrollStart, OffTheTop vp ) ->
            ( History { h | scroll = Scrolling vp }, queryViewport )

        ( ScrollStart, _ ) ->
            ( History h, queryViewport )

        ( BackToTop, _ ) ->
            ( History h, setViewportOf historyElementId 0 0 |> Task.attempt (\_ -> ScrollStart) )

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
    doAfter 50 ViewportResult (getViewportOf historyElementId)


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


rec : History -> Entry -> History
rec (History h) e =
    let
        sanitized =
            sanitizeCtor e
    in
    case h.scroll of
        AtTop ->
            pendingToBuffer (History h) |> pushToBuffer sanitized

        _ ->
            pushToPending sanitized (History h)


sanitizeCtor : Entry -> Entry
sanitizeCtor e =
    { e | ctor = String.replace " " "" e.ctor }


pushToBuffer : Entry -> History -> History
pushToBuffer e (History h) =
    History <|
        case BoundedDeque.popFront h.buffer of
            ( Just top, popped ) ->
                if top.ctor == e.ctor then
                    { h | buffer = BoundedDeque.pushFront e popped }

                else
                    { h | buffer = BoundedDeque.pushFront e h.buffer }

            ( Nothing, _ ) ->
                { h | buffer = BoundedDeque.pushFront e h.buffer }


pushToPending : Entry -> History -> History
pushToPending e (History h) =
    History <|
        case h.pending of
            [] ->
                { h | pending = [ e ] }

            p :: ps ->
                if p.ctor == e.ctor then
                    { h | pending = e :: ps }

                else
                    { h | pending = e :: h.pending }



-- View


historyEl : History -> Element Msg
historyEl h =
    column
        [ width fill
        , padding 10
        , spacing 5
        , BD.rounded 5
        , BG.color oneDark.sub
        , Font.size (scale12 1)
        , inFront (newEntryEl h)
        ]
        [ historyTableEl h
        , msgFiltersEl h
        , payloadFilterInputEl h
        ]


historyTableEl : History -> Element Msg
historyTableEl (History h) =
    table
        [ width fill
        , height (shrink |> maximum 400)
        , padding 5
        , spacing 2
        , clipX
        , BG.color oneDark.main
        , htmlAttribute (id historyElementId)
        , detectScroll (History h)
        ]
        { data =
            let
                ( negMsgFilters, posMsgFilters ) =
                    List.partition (\(MsgFilter isPos _) -> not isPos) h.msgFilters

                payloadQueries =
                    h.payloadFilter |> String.split " " |> List.filter (not << String.isEmpty)
            in
            BoundedDeque.toList h.buffer
                |> List.filter (filterEntry negMsgFilters posMsgFilters payloadQueries)
        , columns = [ ctorColumnEl (History h), payloadColumnEl ]
        }


historyElementId : String
historyElementId =
    "loggerHistory"


detectScroll : History -> Element.Attribute Msg
detectScroll (History h) =
    case h.scroll of
        AtTop ->
            htmlAttribute <| Html.Events.on "scroll" (D.succeed ScrollStart)

        OffTheTop _ ->
            htmlAttribute <| Html.Events.on "scroll" (D.succeed ScrollStart)

        Scrolling _ ->
            noneAttr


filterEntry : List MsgFilter -> List MsgFilter -> List String -> Entry -> Bool
filterEntry negMsgFilters posMsgFilters payloadQueries e =
    if negMsgFilters == [] || not (List.member (MsgFilter False e.ctor) negMsgFilters) then
        if posMsgFilters == [] || List.member (MsgFilter True e.ctor) posMsgFilters then
            payloadQueries == [] || List.any (\q -> List.any (String.contains q) e.payload) payloadQueries

        else
            False

    else
        False


newEntryEl : History -> Element Msg
newEntryEl (History h) =
    case h.pending of
        [] ->
            none

        ps ->
            el [ width fill, alignTop, padding 10 ] <|
                Element.Input.button [ width fill, padding 5, BG.color oneDark.succ ]
                    { onPress = Just BackToTop
                    , label = el [ centerX ] <| text ("New Log Entry (" ++ String.fromInt (List.length ps) ++ ")")
                    }


ctorColumnEl : History -> Column Entry Msg
ctorColumnEl (History h) =
    { header = el [ BG.color oneDark.note ] <| text "Msg"
    , width = fill
    , view =
        \entry ->
            row [ spacing 5 ]
                [ el [ bold ] (text entry.ctor)
                , Element.Input.button [ focused [], htmlAttribute (tabindex -1) ] <|
                    if List.member (MsgFilter True entry.ctor) h.msgFilters then
                        { onPress = Just (DelMsgFilter (MsgFilter True entry.ctor))
                        , label = el [ BG.color oneDark.succ ] <| octiconFreeSizeEl (scale12 1) Octicons.diffAdded
                        }

                    else
                        { onPress = Just (SetMsgFilter (MsgFilter True entry.ctor))
                        , label = el [] <| octiconFreeSizeEl (scale12 1) Octicons.diffAdded
                        }
                , Element.Input.button [ focused [], htmlAttribute (tabindex -1) ] <|
                    -- No need for switch since if Negative Filter is set, this entry should be invisible
                    { onPress = Just (SetMsgFilter (MsgFilter False entry.ctor))
                    , label = el [] <| octiconFreeSizeEl (scale12 1) Octicons.diffRemoved
                    }
                ]
    }


payloadColumnEl : Column Entry Msg
payloadColumnEl =
    { header = el [ BG.color oneDark.note ] <| text "Payload"
    , width = fillPortion 2
    , view =
        \entry ->
            column [ width fill, spacing 2 ] (List.map payloadEl entry.payload)
    }


payloadEl : String -> Element Msg
payloadEl raw =
    el [ width fill, padding 5, BG.color oneDark.bg ] <|
        Element.Input.multiline
            [ width fill
            , height (shrink |> minimum 16 |> maximum 100)
            , padding 0
            , focused []
            , BD.width 0
            , BG.color oneDark.bg
            , Font.family [ Font.typeface "consolas", Font.monospace ]
            , htmlAttribute (readonly True)
            , htmlAttribute (tabindex -1)
            ]
            { onChange = always NoOp
            , text = raw
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Payload"
            , spellcheck = False
            }


msgFiltersEl : History -> Element Msg
msgFiltersEl (History h) =
    wrappedRow [ width fill, spacing 5 ] <|
        List.map msgFilterEl h.msgFilters


msgFilterEl : MsgFilter -> Element Msg
msgFilterEl ((MsgFilter isPos ctor) as mf) =
    row
        [ width shrink
        , BD.rounded 5
        , BG.color (ite isPos oneDark.succ oneDark.err)
        ]
        [ el
            [ padding 2
            , BD.roundEach { topLeft = 5, bottomLeft = 5, topRight = 0, bottomRight = 0 }
            ]
            (text ctor)
        , Element.Input.button
            [ padding 2
            , focused []
            , htmlAttribute (tabindex -1)
            , BD.roundEach { topLeft = 0, bottomLeft = 0, topRight = 5, bottomRight = 5 }
            ]
            { onPress = Just (DelMsgFilter mf)
            , label = octiconFreeSizeEl (scale12 2) Octicons.trashcan
            }
        ]


payloadFilterInputEl : History -> Element Msg
payloadFilterInputEl (History h) =
    Element.Input.text
        [ width fill
        , padding 5
        , BD.width 0
        , BG.color oneDark.note
        , Font.size (scale12 2)
        ]
        { onChange = FilterInput
        , text = h.payloadFilter
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    el [ centerY ] <|
                        text "Payload OR Filter (Space-delimited, Case-sensitive)"
        , label = Element.Input.labelHidden "Log Entry Filter"
        }
