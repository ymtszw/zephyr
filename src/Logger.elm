module Logger exposing (Entry, History, Msg(..), historyEl, init, rec, update)

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
        , filter : String
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
    | SetPosFilter String Bool
    | SetNegFilter String Bool
    | NoOp


init : History
init =
    History
        { buffer = BoundedDeque.empty historyLimit
        , pending = []
        , scroll = AtTop
        , filter = ""
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

        ( FilterInput f, _ ) ->
            ( History { h | filter = f }, Cmd.none )

        ( SetPosFilter f isAdd, _ ) ->
            ( setFilter True f isAdd (History h), Cmd.none )

        ( SetNegFilter f isAdd, _ ) ->
            ( setFilter False f isAdd (History h), Cmd.none )

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


setFilter : Bool -> String -> Bool -> History -> History
setFilter isPos f isAdd (History h) =
    History <|
        case ( isPos, isAdd ) of
            ( True, True ) ->
                if hasPosFilter f (History h) then
                    h

                else if hasNegFilter f (History h) then
                    { h | filter = String.replace ("-" ++ f) f h.filter }

                else
                    { h | filter = StringExtra.appendWithSpace h.filter f }

            ( False, True ) ->
                if hasNegFilter f (History h) then
                    h

                else if hasPosFilter f (History h) then
                    { h | filter = String.replace f ("-" ++ f) h.filter }

                else
                    { h | filter = StringExtra.appendWithSpace h.filter ("-" ++ f) }

            ( True, False ) ->
                if hasPosFilter f (History h) then
                    { h | filter = h.filter |> String.replace (f ++ " ") "" |> String.replace (" " ++ f) "" |> String.replace f "" }

                else
                    h

            ( False, False ) ->
                if hasNegFilter f (History h) then
                    { h | filter = h.filter |> String.replace ("-" ++ f ++ " ") "" |> String.replace (" -" ++ f) "" |> String.replace ("-" ++ f) "" }

                else
                    h


hasNegFilter : String -> History -> Bool
hasNegFilter f (History h) =
    String.contains ("-" ++ f) h.filter


hasPosFilter : String -> History -> Bool
hasPosFilter f (History h) =
    not (hasNegFilter f (History h)) && String.contains f h.filter



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
        , historyFilterInputEl h
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
        { data = BoundedDeque.toList h.buffer |> List.filter (filterEntryBy h.filter)
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


filterEntryBy : String -> Entry -> Bool
filterEntryBy filter e =
    let
        queries =
            String.split " " filter |> List.filter (not << String.isEmpty)
    in
    -- Ensure TCO <https://github.com/elm/compiler/issues/1770>
    filterEntryImpl e [] queries


filterEntryImpl : Entry -> List Bool -> List String -> Bool
filterEntryImpl e posAcc queries =
    case queries of
        [] ->
            case posAcc of
                [] ->
                    True

                _ ->
                    List.any identity posAcc

        q :: qs ->
            case StringExtra.splitAt 1 q of
                [ "-" ] ->
                    filterEntryImpl e posAcc qs

                [ "-", negQ ] ->
                    if String.contains negQ e.ctor || List.any (String.contains negQ) e.payload then
                        -- Negative filter has precedence, so it can exit early. Otherwise check for positive filters.
                        False

                    else
                        filterEntryImpl e posAcc qs

                _ ->
                    if String.contains q e.ctor || List.any (String.contains q) e.payload then
                        filterEntryImpl e (True :: posAcc) qs

                    else
                        filterEntryImpl e (False :: posAcc) qs


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
ctorColumnEl h =
    { header = el [ BG.color oneDark.note ] <| text "Msg"
    , width = fill
    , view =
        \entry ->
            row [ spacing 5 ]
                [ el [ bold ] (text entry.ctor)
                , Element.Input.button [ focused [], htmlAttribute (tabindex -1) ] <|
                    if hasPosFilter entry.ctor h then
                        { onPress = Just (SetPosFilter entry.ctor False)
                        , label = el [ BG.color oneDark.succ ] <| octiconFreeSizeEl (scale12 1) Octicons.diffAdded
                        }

                    else
                        { onPress = Just (SetPosFilter entry.ctor True)
                        , label = el [] <| octiconFreeSizeEl (scale12 1) Octicons.diffAdded
                        }
                , Element.Input.button [ focused [], htmlAttribute (tabindex -1) ] <|
                    if hasNegFilter entry.ctor h then
                        { onPress = Just (SetNegFilter entry.ctor False)
                        , label = el [ BG.color oneDark.succ ] <| octiconFreeSizeEl (scale12 1) Octicons.diffRemoved
                        }

                    else
                        { onPress = Just (SetNegFilter entry.ctor True)
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


historyFilterInputEl : History -> Element Msg
historyFilterInputEl (History h) =
    Element.Input.text
        [ width fill
        , padding 5
        , BD.width 0
        , BG.color oneDark.note
        , Font.size (scale12 2)
        ]
        { onChange = FilterInput
        , text = h.filter
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    el [ centerY ] <|
                        text "OR Filter (Space-delimited, Case-sensitive, Negate with '-' prefix)"
        , label = Element.Input.labelHidden "Log Entry Filter"
        }
